{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
module Cabal2Pkg.Utils
  ( unsafeEncodeUtf
  ) where

import Prelude.Unicode ((∘))
#if !MIN_VERSION_filepath(1, 5, 2)
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.OsPath qualified as OP
import System.OsPath (OsPath)


-- A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf ∷ String → OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = unsafePerformIO ∘ OP.encodeUtf
#endif
