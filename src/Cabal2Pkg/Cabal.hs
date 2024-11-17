{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Text (Text, isPrefixOf)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec.Warning (PWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.OsPath (OsPath)
import System.OsPath qualified as OP
import System.OsPath.Internal qualified as OPI


readCabal :: Text -- ^ URL of .tar.gz file, or a path to local .tar.gz file
          -> CLI GenericPackageDescription
readCabal url =
  do (cabalPath, ws, cabal) <-
       do mb <- runConduit
               $ gzippedTarball url .| ungzip .| untar findCabal .| C.head
          case mb of
            Nothing ->
              fatal $ "Can't find any .cabal files in " <> url
            Just cabal ->
              pure cabal
     mapM_ (warn' cabalPath) ws
     pure cabal
  where
    warn' :: OsPath -> PWarning -> CLI ()
    warn' path w =
      do path' <- T.pack <$> OP.decodeUtf path
         warn $ path' <> ": " <> T.pack (show w)

gzippedTarball :: MonadResource m => Text -> ConduitT i ByteString m ()
gzippedTarball url
  | "https://" `isPrefixOf` url =
      error "FIXME"
  | otherwise =
      -- Assume it's a local file.
      sourceFile $ T.unpack url

findCabal :: (MonadFail m, MonadThrow m)
          => FileInfo
          -> ConduitT ByteString (OsPath, [PWarning], GenericPackageDescription) m ()
findCabal fi =
  do path <- OPI.fromBytes $ filePath fi
     case OP.splitPath path of
       [_root, file]
         | ".cabal" `OP.isExtensionOf` file ->
             do res <- DPP.parseGenericPackageDescription . toStrict <$> sinkLazy
                case DPP.runParseResult res of
                  (_, Left e) ->
                    fail ("Cannot parse " <> show path <> ": " <> show e)
                  (ws, Right cabal) ->
                    yield (path, ws, cabal)
       _ ->
         pure ()
