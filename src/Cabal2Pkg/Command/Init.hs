{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Utils qualified as Utils
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit ((.|), ConduitT, await, runConduit, yield)
import Data.Conduit.Combinators (sourceFile)
import Data.Conduit.Tar (FileInfo(filePath), untar)
import Data.List (isPrefixOf)
import System.OsPath (OsPath, isExtensionOf, splitPath)
import System.OsPath.Internal qualified as OPI


run ∷ ∀m. (MonadIO m, MonadResource m, MonadThrow m) ⇒ OsPath → String → m ()
run dir url =
  do cabal ← runConduit
             $ gzippedTarball url .| untar findCabalFile .| await
     error "FIXME"

gzippedTarball ∷ MonadResource m ⇒ String → ConduitT i ByteString m ()
gzippedTarball url
  | "https://" `isPrefixOf` url =
      error "FIXME"
  | otherwise =
      -- Assume it's a local file.
      sourceFile url

findCabalFile ∷ MonadThrow m ⇒ FileInfo → ConduitT ByteString ByteString m ()
findCabalFile fi =
  do path ← OPI.fromBytes $ filePath fi
     case splitPath path of
       [_root, file]
         | (Utils.unsafeEncodeUtf ".cabal") `isExtensionOf` file →
             -- XXX: Is there not a combinator for this?
             do contents ← await
                case contents of
                  Just c  → yield c
                  Nothing → return ()
       _ →
         return ()
