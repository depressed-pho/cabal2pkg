{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Cabal2Pkg.Cabal
  ( readCabal
  ) where

import Cabal2Pkg.CmdLine (CLI, fatal, warn)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit ((.|), ConduitT, runConduit, yield)
import Data.Conduit.Combinators (sinkLazy, sourceFile)
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Tar (FileInfo(filePath), untar)
import Data.Conduit.Zlib (ungzip)
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec (eitherParsec)
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageId qualified as C
import Network.URI (URI(..))
import PackageInfo_cabal2pkg qualified as PI
import Prelude hiding (pi)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OP
import System.OsPath.Internal qualified as OPI


readCabal :: URI -- ^URI of a package tarball
          -> CLI GenericPackageDescription
readCabal uri =
  do (cabalPath, ws, cabal) <-
       do mb <- runConduit
                $ gzippedTarball uri .| ungzip .| untar findCabal .| C.head
          case mb of
            Nothing ->
              fatal $ "Can't find any .cabal files in" <+> PP.viaShow uri
            Just cabal ->
              pure cabal
     mapM_ (warn' cabalPath) ws
     pure cabal
  where
    warn' :: OsPath -> PWarning -> CLI ()
    warn' path w =
      do path' <- OP.decodeUtf path
         warn . PP.pretty $ showPWarning path' w

gzippedTarball :: (MonadResource m, MonadThrow m)
               => URI
               -> ConduitT i ByteString m ()
gzippedTarball uri =
  -- NOTE: This is technically wrong. RFC 3986 says URI schemes are case
  -- insensitive. But everyone wrongly treats them as case sensitive so...
  case uriScheme uri of
    "http:"  -> fetchHTTP uri
    "https:" -> fetchHTTP uri
    "file:"  -> fetchLocal uri
    ""       -> fetchHackage uri
    _        -> fetchUnknown

fetchHTTP :: MonadResource m => URI -> ConduitT i ByteString m ()
fetchHTTP _uri =
  error "FIXME"

fetchHackage :: (MonadResource m, MonadThrow m) => URI -> ConduitT i ByteString m ()
fetchHackage (uriPath -> path) =
  case eitherParsec path of
    Left _   -> fetchUnknown
    Right pi -> error ("FIXME: " <> show (pi :: C.PackageId))

fetchLocal :: MonadResource m => URI -> ConduitT i ByteString m ()
fetchLocal uri =
  -- NOTE: According to RFC 8089, technically we should verify that the
  -- authority part of the file:// URI is either empty, "localhost", or the
  -- FQDN of the local host. But is that worth it?
  sourceFile . uriPath $ uri

fetchUnknown :: MonadThrow m => m a
fetchUnknown =
  fatal $ PP.vsep
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
  , PP.hsep [ "- a schema-less name of a package to be retrieved from Hackage,"
            , "in the form of"
            , PP.annotate styForm "NAME"
            , eg "cabal-install"
            , "or"
            , PP.annotate styForm "NAME-VERSION"
            , eg "cabal-install-3.10.3.0"
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

findCabal :: (MonadFail m, MonadThrow m)
          => FileInfo
          -> ConduitT ByteString (OsPath, [PWarning], GenericPackageDescription) m ()
findCabal fi =
  do path <- OPI.fromBytes $ filePath fi
     case OP.splitPath path of
       [_root, file]
         | [osp|.cabal|] `OP.isExtensionOf` file ->
             do res <- DPP.parseGenericPackageDescription . toStrict <$> sinkLazy
                case DPP.runParseResult res of
                  (_, Left e) ->
                    fail ("Cannot parse " <> show path <> ": " <> show e)
                  (ws, Right cabal) ->
                    yield (path, ws, cabal)
       _ ->
         pure ()
