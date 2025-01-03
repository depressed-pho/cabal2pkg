{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Site
  ( PackageURI(..)
  , parsePackageURI
  , reconstructPackageURI
  , renderPackageURI
  , isFromLocalFS
  , isFromHackage
  ) where

import Cabal2Pkg.CmdLine (CLI, fatal)
import Cabal2Pkg.Site.GitHub
  ( GitHubDist, parseGitHubDist, reconstructGitHubDist, renderGitHubDist )
import Cabal2Pkg.Site.GitLab
  ( GitLabDist, parseGitLabDist, reconstructGitLabDist, renderGitLabDist )
import Cabal2Pkg.Site.Hackage
  ( HackageDist, parseHackageDist, reconstructHackageDist, renderHackageDist )
import Control.Applicative (asum)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Extra (maybeM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Data (Data)
import Data.Maybe (fromMaybe, isJust)
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Types.PackageName (PackageName)
import Lens.Micro ((&), (%~))
import Network.URI (URI(..))
import Network.URI.Lens (uriPathLens)
import PackageInfo_cabal2pkg qualified as PI
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.OsPath.Posix qualified as OP


-- We don't support FTP because we don't want to bring in an FTP client as
-- a dependency. Is there even one?
data PackageURI =
    HTTP    !URI
  | File    !FilePath
  | GitHub  !GitHubDist
  | GitLab  !GitLabDist
  | Hackage !HackageDist
  deriving (Data, Eq, Show)

parsePackageURI :: Maybe PackageName -- ^Context, used for the @update@ command
                -> URI
                -> CLI PackageURI
parsePackageURI ctx uri =
  case uriScheme uri of
    "http:"  -> parseHTTPURI ctx uri
    "https:" -> parseHTTPURI ctx uri
    "file:"  -> parseFileURI uri
    ""       -> maybeM (parseUnknown ctx) (pure . Hackage) (parseHackageDist ctx uri)
    _        -> parseUnknown ctx

parseHTTPURI :: Maybe PackageName -> URI -> CLI PackageURI
parseHTTPURI ctx uri =
  do gitHub  <- (GitHub  <$>) <$> parseGitHubDist uri
     gitLab  <- (GitLab  <$>) <$> parseGitLabDist uri
     hackage <- (Hackage <$>) <$> parseHackageDist ctx uri
     pure . fromMaybe (HTTP uri) $ asum [gitHub, gitLab, hackage]

parseFileURI :: Applicative m => URI -> m PackageURI
parseFileURI =
  -- THINKME: According to RFC 8089, technically we should verify that the
  -- authority part of the file:// URI is either empty, "localhost", or the
  -- FQDN of the local host. But is that worth it?
  pure . File . uriPath

parseUnknown :: MonadThrow m => Maybe PackageName -> m a
parseUnknown ctx =
  fatal . PP.vsep $
  [ PP.hsep [ PP.pretty PI.name
            , "doesn't know how to handle this URI. These are all it supports:"
            ]
  , PP.hsep [ PP.pretty '-'
            , PP.annotate styScheme "http://"
            , "or"
            , PP.annotate styScheme "https://"
            , "URL of a package tarball"
            ]
  , PP.hsep [ PP.pretty '-'
            , PP.annotate styScheme "file://"
            , "URL of a package tarball on the local filesystem"
            ]
    -- NOTE: It would be nice to render the word "Hackage" as a hyperlink
    -- to https://hackage.haskell.org/, but sadly
    -- prettyprinter-ansi-terminal doesn't support hyperlinks at the
    -- moment.
  ]
  <> if isJust ctx then
       [ PP.hsep [ "- the version to be retrieved from Hackage"
                 , eg "0.1.2"
                 ]
       ]
     else
       [ PP.hsep [ "- the name of a package to be retrieved from Hackage,"
                 , "in the form of"
                 , PP.annotate styForm "NAME"
                 , eg "foo"
                 , "or"
                 , PP.annotate styForm "NAME-VERSION"
                 , eg "foo-0.1.2"
                 ]
       ]
  where
    styScheme :: AnsiStyle
    styScheme = PP.colorDull PP.Green

    styForm :: AnsiStyle
    styForm = PP.colorDull PP.Cyan

    styExample :: AnsiStyle
    styExample = PP.colorDull PP.Yellow

    eg :: Doc AnsiStyle -> Doc AnsiStyle
    eg = PP.parens . ("e.g." <+>) . PP.annotate styExample

-- |Try to reconstruct a 'PackageURI' from an existing pkgsrc
-- package. Return 'Nothing' if it has no @MASTER_SITES@.
reconstructPackageURI :: Package CLI -> CLI (Maybe PackageURI)
reconstructPackageURI pkg =
  do http    <- (HTTP    <$>) <$> reconstructHTTPURI     pkg
     -- file URI can never be reconstructed.
     gitHub  <- (GitHub  <$>) <$> reconstructGitHubDist  pkg
     gitLab  <- (GitLab  <$>) <$> reconstructGitLabDist  pkg
     hackage <- (Hackage <$>) <$> reconstructHackageDist pkg
     pure $ asum [gitHub, gitLab, hackage, http]

reconstructHTTPURI :: (MonadThrow m, MonadUnliftIO m) => Package m -> m (Maybe URI)
reconstructHTTPURI pkg =
  do ms <- SrcDb.masterSites pkg
     case ms of
       (m:_)
         | uriScheme m == "http:" || uriScheme m == "https:" ->
             do distName <- OP.decodeUtf =<< SrcDb.distName    pkg
                sufx     <- OP.decodeUtf =<< SrcDb.extractSufx pkg
                pure . Just $ m & uriPathLens %~ \base -> base <> distName <> sufx
       _ ->
         pure Nothing

renderPackageURI :: PackageURI -> CLI URI
renderPackageURI (HTTP uri ) = pure uri
renderPackageURI (File path) =
  pure $ URI { uriScheme    = "file:"
             , uriAuthority = Nothing
             , uriPath      = path
             , uriQuery     = ""
             , uriFragment  = ""
             }
renderPackageURI (GitLab  dist) = renderGitLabDist dist
renderPackageURI (GitHub  dist) = renderGitHubDist dist
renderPackageURI (Hackage dist) = pure $ renderHackageDist dist

isFromLocalFS :: PackageURI -> Bool
isFromLocalFS File {} = True
isFromLocalFS _       = False

isFromHackage :: PackageURI -> Bool
isFromHackage Hackage {} = True
isFromHackage _          = False
