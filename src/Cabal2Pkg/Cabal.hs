{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal2Pkg.Cabal
  ( readCabal
  ) where

import Cabal2Pkg.PackageURI (PackageURI(..))
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
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Lens.Micro ((&), (%~))
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpSource, parseRequest )
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as MT
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), uriToString)
import Network.URI.Lens (uriPathLens)
import Prelude hiding (pi)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OPP
import System.OsString.Posix qualified as OSP


readCabal :: PackageURI -> CLI GenericPackageDescription
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

fetchCabal :: (MonadResource m, MonadThrow m, PrimMonad m)
           => URI -- ^Hackage URI
           -> PackageURI
           -> ConduitT i (PosixPath, ByteString) m ()
fetchCabal _   (HTTP    uri      ) = fetchHTTP uri
fetchCabal _   (File    path     ) = fetchLocal path
fetchCabal hkg (Hackage name mVer) = fetchHackage hkg name mVer

fetchLocal :: (MonadResource m, MonadThrow m, PrimMonad m)
           => FilePath
           -> ConduitT i (PosixPath, ByteString) m ()
fetchLocal path =
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
             -> PackageName
             -> Maybe Version
             -> ConduitT i (PosixPath, ByteString) m ()
fetchHackage hackage name mVer =
  do req   <- parseRequest $ uriToString id cabalURI ""
     cabal <- toStrict <$> (httpSource req getCabal .| sinkLazy)
     file  <- OPP.encodeUtf cabalFile
     yield (file, cabal)
  where
    -- We generate
    -- https://hackage.haskell.org/package/{packageId}/revision/0.cabal but
    -- not https://hackage.haskell.org/package/{packageId}.cabal, because
    -- the former is exactly what in downloaded tarballs. The latter is
    -- potentially a revised one.
    cabalURI :: URI
    cabalURI =
      hackage & uriPathLens %~ \base ->
      FP.joinPath [ base
                  , "package"
                  , prettyShow name <> maybe mempty (("-" <>) . prettyShow) mVer
                  , "revision"
                  , "0.cabal"
                  ]

    cabalFile :: FilePath
    cabalFile =
      mconcat [ prettyShow name
              , maybe mempty (("-" <>) . prettyShow) mVer
              , ".cabal"
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
