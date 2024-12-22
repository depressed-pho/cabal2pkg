{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal2Pkg.Cabal
  ( readCabal
  ) where

import Cabal2Pkg.Hackage qualified as Hackage
import Cabal2Pkg.PackageURI (PackageURI(..))
import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.CmdLine (CLI, fatal, warn)
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
import Data.Text.Encoding (decodeUtf8Lenient)
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpSource, parseRequest )
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as MT
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), uriToString)
import Prelude hiding (pi)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OP
import System.OsString.Posix qualified as OS


readCabal :: PackageURI -> CLI GenericPackageDescription
readCabal uri =
  do (cabalPath, ws, gpd) <-
       do mbCabal <- runConduit $ fetchCabal uri .| C.head
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
          fatal ( "Cannot parse" <+>
                  prettyAnsi cabalPath <>
                  PP.colon <+>
                  PP.viaShow e )
        (ws, Right gpd) ->
          pure (cabalPath, ws, gpd)

    warn' :: PosixPath -> PWarning -> CLI ()
    warn' path w =
      do path' <- OP.decodeUtf path
         warn . PP.pretty $ showPWarning path' w

fetchCabal :: PackageURI
           -> ConduitT i (PosixPath, ByteString) CLI ()
fetchCabal (HTTP    uri      ) = fetchHTTP uri
fetchCabal (File    path     ) = fetchLocal path
fetchCabal (Hackage name mVer) = Hackage.fetchCabal name mVer

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
      do path <- OS.fromBytes . filePath $ fi
         case OP.splitPath path of
           [_root, file]
             | [pstr|.cabal|] `OP.isExtensionOf` file ->
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
                    prettyAnsi uri <>
                    PP.colon <+>
                    "Bad media type:" <+>
                    PP.viaShow ts )
      else
        let sc = getResponseStatus res
        in
          fatal ( "Couldn't fetch a package tarball from" <+>
                  prettyAnsi uri <>
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
