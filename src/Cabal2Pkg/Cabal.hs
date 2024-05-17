{-# LANGUAGE ScopedTypeVariables #-}
module Cabal2Pkg.Cabal
  ( readCabal
  ) where

import Cabal2Pkg.Utils qualified as Utils
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit ((.|), ConduitT, runConduit, yield)
import Data.Conduit.Combinators (sinkLazy, sourceFile)
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Tar (FileInfo(filePath), untar)
import Data.Conduit.Zlib (ungzip)
import Data.List (isPrefixOf)
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec.Warning (PWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.IO (stderr, hPutStr, hPrint)
import System.OsPath (OsPath, isExtensionOf, splitPath)
import System.OsPath.Internal qualified as OPI


readCabal :: forall m. ( MonadFail m
                       , MonadIO m
                       , MonadResource m
                       , MonadThrow m
                       , PrimMonad m
                       )
          => String -- ^ URL of .tar.gz file, or a path to local .tar.gz file
          -> m GenericPackageDescription
readCabal url =
  do (cabalPath, ws, cabal) <-
       do mb <- runConduit
               $ gzippedTarball url .| ungzip .| untar findCabal .| C.head
          case mb of
            Nothing ->
              fail ("Can't find any .cabal files in " <> url)
            Just cabal ->
              pure cabal
     mapM_ (warn cabalPath) ws
     pure cabal
  where
    warn :: OsPath -> PWarning -> m ()
    warn path w =
      liftIO $
      do hPutStr stderr (show path <> ": warning: ")
         hPrint stderr w

gzippedTarball :: MonadResource m => String -> ConduitT i ByteString m ()
gzippedTarball url
  | "https://" `isPrefixOf` url =
      error "FIXME"
  | otherwise =
      -- Assume it's a local file.
      sourceFile url

findCabal :: (MonadFail m, MonadThrow m)
          => FileInfo
          -> ConduitT ByteString (OsPath, [PWarning], GenericPackageDescription) m ()
findCabal fi =
  do path <- OPI.fromBytes $ filePath fi
     case splitPath path of
       [_root, file]
         | (Utils.unsafeEncodeUtf ".cabal") `isExtensionOf` file ->
             do res <- DPP.parseGenericPackageDescription . toStrict <$> sinkLazy
                case DPP.runParseResult res of
                  (_, Left e) ->
                    fail ("Cannot parse " <> show path <> ": " <> show e)
                  (ws, Right cabal) ->
                    yield (path, ws, cabal)
       _ ->
         pure ()
