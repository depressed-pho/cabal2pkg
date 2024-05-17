{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource)
import System.OsPath (OsPath)


run ∷ ∀m. ( MonadFail m
          , MonadIO m
          , MonadResource m
          , MonadThrow m
          , PrimMonad m
          )
    ⇒ OsPath
    → String -- ^ URL of .tar.gz file
    → m ()
run dir url =
  do cabal ← readCabal url
     fail (show genMakefile)
