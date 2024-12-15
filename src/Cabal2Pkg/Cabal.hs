{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Cabal2Pkg.Cabal
  ( isFromHackage
  , readCabal
  ) where

import Cabal2Pkg.CmdLine (CLI, hackageURI, fatal, warn)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit ((.|), ConduitT, runConduit, yield)
import Data.Conduit.Combinators (sinkLazy, sourceFile)
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Tar (FileInfo(filePath), untar)
import Data.Conduit.Zlib (ungzip)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec (eitherParsec)
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageId (PackageId)
import Lens.Micro ((&), (%~))
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpSource, parseRequest )
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as MT
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), pathSegments, uriToString)
import Network.URI.Lens (uriPathLens)
import PackageInfo_cabal2pkg qualified as PI
import Prelude hiding (pi)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OPP
import System.OsString.Posix qualified as OSP


readCabal :: URI -- ^URI of a package tarball
          -> CLI GenericPackageDescription
readCabal uri =
  do (cabalPath, ws, gpd) <-
       do hackage <- hackageURI
          mbCabal <- runConduit $ fetchCabal hackage uri .| C.head
          case mbCabal of
            Nothing ->
              fatal $ "Can't find any .cabal files in" <+> PP.viaShow uri
            Just (cabalPath, cabal) ->
              parseCabal cabalPath cabal
     mapM_ (warn' cabalPath) ws
     pure gpd
  where
    parseCabal :: MonadThrow m
               => PosixPath
               -> ByteString
               -> m (PosixPath, [PWarning], GenericPackageDescription)
    parseCabal cabalPath cabal =
      case DPP.runParseResult $ DPP.parseGenericPackageDescription cabal of
        (_, Left e) ->
          do path' <- T.pack <$> OPP.decodeUtf cabalPath
             fatal ( "Cannot parse" <+>
                     PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.pretty path')) <>
                     PP.colon <+>
                     PP.viaShow e )
        (ws, Right gpd) ->
          pure (cabalPath, ws, gpd)

    warn' :: PosixPath -> PWarning -> CLI ()
    warn' path w =
      do path' <- OPP.decodeUtf path
         warn . PP.pretty $ showPWarning path' w

isFromHackage :: URI -> CLI Bool
isFromHackage uri =
  case uriScheme uri of
    "" -> pure True
    _  -> do hackage <- hackageURI
             pure $ hackage `isPrefixOf` uri
  where
    isPrefixOf :: URI -> URI -> Bool
    isPrefixOf a b =
      uriScheme    a == uriScheme    b &&
      uriAuthority a == uriAuthority b &&
      (pathSegments a <> ["package"]) `L.isPrefixOf` pathSegments b

fetchCabal :: (MonadResource m, MonadThrow m, PrimMonad m)
           => URI -- ^Hackage URI
           -> URI -- ^User-specified URI
           -> ConduitT i (PosixPath, ByteString) m ()
fetchCabal hackage uri =
  -- NOTE: This is technically wrong. RFC 3986 says URI schemes are case
  -- insensitive. But everyone wrongly treats them as case sensitive so...
  case uriScheme uri of
    "http:"  -> fetchHTTP uri
    "https:" -> fetchHTTP uri
    "file:"  -> fetchLocal uri
    ""       -> fetchHackage hackage uri
    _        -> fetchUnknown

fetchLocal :: (MonadResource m, MonadThrow m, PrimMonad m)
           => URI
           -> ConduitT i (PosixPath, ByteString) m ()
fetchLocal (uriPath -> path) =
  -- NOTE: According to RFC 8089, technically we should verify that the
  -- authority part of the file:// URI is either empty, "localhost", or the
  -- FQDN of the local host. But is that worth it?
  sourceFile path .| extractCabalFromTarball

extractCabalFromTarball :: (MonadResource m, MonadThrow m, PrimMonad m)
                        => ConduitT ByteString (PosixPath, ByteString) m ()
extractCabalFromTarball = ungzip .| untar findCabal
  where
    findCabal :: MonadThrow m
              => FileInfo
              -> ConduitT ByteString (PosixPath, ByteString) m ()
    findCabal fi =
      do path <- OSP.fromBytes . filePath $ fi
         case OPP.splitPath path of
           [_root, file]
             | [pstr|.cabal|] `OPP.isExtensionOf` file ->
                 do cabal <- toStrict <$> sinkLazy
                    yield (path, cabal)
           _ ->
             pure ()

fetchHTTP :: (MonadResource m, MonadThrow m, PrimMonad m)
          => URI
          -> ConduitT i (PosixPath, ByteString) m ()
fetchHTTP uri =
  do req <- parseRequest $ uriToString id uri ""
     httpSource req getTarball .| extractCabalFromTarball
  where
    getTarball :: MonadThrow m
               => Response (ConduitT i ByteString m ())
               -> ConduitT i ByteString m ()
    getTarball res =
      if statusIsSuccessful . getResponseStatus $ res then
        case getResponseHeader hContentType res of
          [cType]
            | isTarball cType ->
                getResponseBody res
          ts ->
            fatal ( "Couldn't fetch a package tarball from" <+>
                    PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.viaShow uri)) <>
                    PP.colon <+>
                    "Bad media type:" <+>
                    PP.viaShow ts )
      else
        let sc = getResponseStatus res
        in
          fatal ( "Couldn't fetch a package tarball from" <+>
                  PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.viaShow uri)) <>
                  PP.colon <+>
                  PP.pretty (statusCode sc) <+>
                  PP.pretty (decodeUtf8Lenient . statusMessage $ sc) )

    isTarball :: ByteString -> Bool
    isTarball cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> any (MT.matches mt) tarballTypes

    tarballTypes :: [MediaType]
    tarballTypes =
      [ "application/gzip"       -- RFC 6713
      , "application/tar+gzip"   -- Non-standard
      , "application/x-gzip"     -- Non-standard
      , "application/x-tar+gzip" -- Non-standard
      ]

-- Hackage recommends API users to use the hackage-security library instead
-- of directly accessing it. However, hackage-security is designed to build
-- a clone of the entire database as a local cache and update it from time
-- to time. That doesn't really suit well to our use case.
fetchHackage :: (MonadResource m, MonadThrow m)
             => URI -- ^Hackage URI
             -> URI -- ^User-specified URI
             -> ConduitT i (PosixPath, ByteString) m ()
fetchHackage hackage (uriPath -> packageId) =
  case eitherParsec packageId of
    Left _   -> fetchUnknown
    Right pi ->
      do let uri = cabalURI pi
         req   <- parseRequest $ uriToString id uri ""
         cabal <- toStrict <$> (httpSource req getCabal .| sinkLazy)
         file  <- OPP.encodeUtf $ prettyShow pi <> ".cabal"
         yield (file, cabal)
  where
    -- We generate
    -- https://hackage.haskell.org/package/{packageId}/revision/0.cabal but
    -- not https://hackage.haskell.org/package/{packageId}.cabal, because
    -- the former is exactly what in downloaded tarballs. The latter is
    -- potentially a revised one.
    cabalURI :: PackageId -> URI
    cabalURI pi =
      hackage & uriPathLens %~ \base ->
      FP.joinPath [ base
                  , "package"
                  , prettyShow pi
                  , "revision"
                  , "0.cabal"
                  ]

    getCabal :: MonadThrow m
             => Response (ConduitT i ByteString m ())
             -> ConduitT i ByteString m ()
    getCabal res =
      if statusIsSuccessful . getResponseStatus $ res then
        case getResponseHeader hContentType res of
          [cType]
            | isCabal cType ->
                getResponseBody res
          ts ->
            fatal ( "Couldn't fetch a package description from Hackage:" <+>
                    "Bad media type:" <+>
                    PP.viaShow ts )
      else
        let sc = getResponseStatus res
        in
          fatal ( "Couldn't fetch a package description from Hackage:" <+>
                  PP.pretty (statusCode sc) <+>
                  PP.pretty (decodeUtf8Lenient . statusMessage $ sc) )

    isCabal :: ByteString -> Bool
    isCabal cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> MT.mainType mt == "text"  &&
                   MT.subType  mt == "plain" &&
                   elem (mt MT./. "charset") [Nothing, Just "utf-8"]

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
