{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Site.GitHub
  ( -- * URI
    GitHubDist(..)
  , parseGitHubDist
  , reconstructGitHubDist
  , renderGitHubDist

    -- * Makefile
  , genGitHubMasterSites
  )
  where

import Cabal2Pkg.CmdLine (CLI, gitHubURI)
import Cabal2Pkg.Site.Common (isIn)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.Data (Data)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import Language.BMake.AST (Block(..))
import Language.BMake.AST.Plain ((.=), PlainAST)
import Lens.Micro.Platform ((%~))
import Network.URI (URI, pathSegments)
import Network.URI.Lens (uriPathLens)
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix qualified as OP


data GitHubDist =
    Tag
    { ghAccount  :: !Text
    , ghProject  :: !Text
    , ghTag      :: !Text
    }
  | Release
    { ghAccount  :: !Text
    , ghProject  :: !Text
    , ghRelease  :: !Text
    , ghDistName :: !Text
    }
  deriving (Data, Eq, Show)

-- |Try to parse a GitHub archive URI. Return 'Nothing' if it cannot be parsed.
parseGitHubDist :: URI -> CLI (Maybe GitHubDist)
parseGitHubDist uri = go <$> gitHubURI
  where
    go :: URI -> Maybe GitHubDist
    go gitHub =
      do unless (uri `isIn` gitHub) $
           fail "clearly not a GitHub URI"
         let nDrop = L.length (pathSegments gitHub)
             segs  = pathSegments uri
         case L.drop nDrop segs of
           [account, project, "archive", file] ->
             do tag <- FP.stripExtension ".tar.gz" file
                pure $ Tag { ghAccount = T.pack account
                           , ghProject = T.pack project
                           , ghTag     = T.pack tag
                           }
           [account, project, "archive", "refs", "tags", file] ->
             do tag <- FP.stripExtension ".tar.gz" file
                pure $ Tag { ghAccount = T.pack account
                           , ghProject = T.pack project
                           , ghTag     = T.pack $ FP.joinPath ["refs", "tags", tag]
                           }
           [account, project, "releases", "download", release, file] ->
             do distName <- FP.stripExtension ".tar.gz" file
                pure $ Release { ghAccount  = T.pack account
                               , ghProject  = T.pack project
                               , ghRelease  = T.pack release
                               , ghDistName = T.pack distName
                               }
           _ -> fail "unknown GitHub URI"

-- |Try to reconstruct a 'GitHubDist' from an existing pkgsrc
-- package. Return 'Nothing' if it's not from GitHub.
reconstructGitHubDist :: Package CLI -> CLI (Maybe GitHubDist)
reconstructGitHubDist pkg =
  runMaybeT $
  do gitHub  <- lift gitHubURI
     (uri:_) <- lift $ SrcDb.masterSites pkg
     unless (uri `isIn` gitHub) $
       fail "clearly not from GitHub"
     let nDrop = L.length (pathSegments gitHub)
         segs  = pathSegments uri
     [account] <- pure . (T.pack <$>) $ L.drop nDrop segs
     project   <- hoistMaybe =<< lift (SrcDb.gitHubProject pkg)
     typ       <- hoistMaybe =<< lift (SrcDb.gitHubType    pkg)
     case typ of
       SrcDb.GitHubTag ->
         do tag <- hoistMaybe =<< lift (SrcDb.gitHubTag pkg)
            pure $ Tag { ghAccount = account
                       , ghProject = project
                       , ghTag     = tag
                       }
       SrcDb.GitHubRelease ->
         do release  <- hoistMaybe =<< lift (SrcDb.gitHubRelease pkg)
            distName <- lift $ (T.pack <$>) . OP.decodeUtf =<< SrcDb.distName pkg
            pure $ Release { ghAccount  = account
                           , ghProject  = project
                           , ghRelease  = release
                           , ghDistName = distName
                           }

renderGitHubDist :: GitHubDist -> CLI URI
renderGitHubDist (Tag {..}) =
  (uriPathLens %~ f) <$> gitHubURI
  where
    f base = FP.joinPath [ base
                         , T.unpack ghAccount
                         , T.unpack ghProject
                         , "archive"
                         , T.unpack ghTag <> ".tar.gz"
                         ]
renderGitHubDist (Release {..}) =
  (uriPathLens %~ f) <$> gitHubURI
  where
    f base = FP.joinPath [ base
                         , T.unpack ghAccount
                         , T.unpack ghProject
                         , "releases"
                         , "download"
                         , T.unpack ghRelease
                         , T.unpack ghDistName <> ".tar.gz"
                         ]

-- |See pkgsrc @mk/fetch/github.mk@
genGitHubMasterSites :: Text -> Version -> GitHubDist -> [Block PlainAST]
genGitHubMasterSites pkgBase ver dist =
  [ "MASTER_SITES" .= ["${MASTER_SITE_GITHUB:=" <> ghAccount dist <> "/}"] ]
  <> [ "GITHUB_PROJECT" .= [ghProject dist]
     | ghProject dist /= pkgBase
     ]
  <> case dist of
       Tag {..}
         | ghTag == ver' -> [] -- Use the default value.
         | otherwise     ->
             [ "GITHUB_TAG" .= [T.replace ver' "${PKGVERSION_NOREV}" ghTag] ]
             <> [ "WRKSRC"  .= pure "${WRKDIR}/${GITHUB_PROJECT}-${PKGVERSION_NOREV}"
                | "refs/tags/" `T.isPrefixOf` ghTag
                ]
       Release {..} ->
         [ "GITHUB_RELEASE" .= pure ghRelease ]
  where
    ver' :: Text
    ver' = T.pack . prettyShow $ ver
