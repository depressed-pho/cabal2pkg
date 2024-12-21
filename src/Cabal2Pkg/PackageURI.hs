{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal2Pkg.PackageURI
  ( PackageURI(..)
  , parsePackageURI
  , renderPackageURI
  , isFromHackage
  ) where

import Cabal2Pkg.CmdLine (CLI, fatal, hackageURI)
import Control.Exception.Safe (MonadThrow)
import Data.List qualified as L
import Data.Maybe (isJust)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier(pkgName, pkgVersion))
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Network.URI (URI(..), pathSegments)
import PackageInfo_cabal2pkg qualified as PI
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP


data PackageURI =
    HTTP    !URI
  | File    !FilePath
  | Hackage !PackageName !(Maybe Version)

instance Show PackageURI where
  show = show . renderPackageURI

parsePackageURI :: MonadThrow m
                => Maybe PackageName -- ^Context, used for the @update@ command
                -> URI
                -> m PackageURI
parsePackageURI ctx uri =
  case uriScheme uri of
    "http:"  -> pure $ HTTP uri
    "https:" -> pure $ HTTP uri
    "file:"  -> parseFileURI uri
    ""       -> parseHackageURI ctx uri
    _        -> parseUnknown ctx

parseFileURI :: Applicative m => URI -> m PackageURI
parseFileURI =
  -- THINKME: According to RFC 8089, technically we should verify that the
  -- authority part of the file:// URI is either empty, "localhost", or the
  -- FQDN of the local host. But is that worth it?
  pure . File . uriPath

parseHackageURI :: MonadThrow m => Maybe PackageName -> URI -> m PackageURI
parseHackageURI ctx@Nothing (uriPath -> path) =
  case eitherParsec path of
    Right pkgId ->
      -- It's a full package ID, i.e. NAME-VERSION
      pure $ Hackage (pkgName pkgId) (Just $ pkgVersion pkgId)
    Left _ ->
      case eitherParsec path of
        Right name ->
          -- It's a package name without version
          pure $ Hackage name Nothing
        Left _ ->
          parseUnknown ctx
parseHackageURI ctx@(Just name) (uriPath -> path) =
  case eitherParsec path of
    Right ver ->
      -- It's a version
      pure $ Hackage name (Just ver)
    Left _ ->
      parseUnknown ctx

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

renderPackageURI :: PackageURI -> URI
renderPackageURI (HTTP uri ) = uri
renderPackageURI (File path) =
  URI { uriScheme    = "file:"
      , uriAuthority = Nothing
      , uriPath      = path
      , uriQuery     = ""
      , uriFragment  = ""
      }
renderPackageURI (Hackage name mVer) =
  URI { uriScheme    = ""
      , uriAuthority = Nothing
      , uriPath      = prettyShow name <> maybe mempty (("-" <>) . prettyShow) mVer
      , uriQuery     = ""
      , uriFragment  = ""
      }

isFromHackage :: PackageURI -> CLI Bool
isFromHackage (File    _  ) = pure False
isFromHackage (Hackage _ _) = pure True
isFromHackage (HTTP    uri) =
  do hackage <- hackageURI
     pure $ hackage `isPrefixOf` uri
  where
    isPrefixOf :: URI -> URI -> Bool
    isPrefixOf a b =
      uriScheme    a == uriScheme    b &&
      uriAuthority a == uriAuthority b &&
      (pathSegments a <> ["package"]) `L.isPrefixOf` pathSegments b
