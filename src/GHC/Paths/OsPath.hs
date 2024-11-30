{-# LANGUAGE CPP #-}
-- |This is an alternative API for ghc-paths based on the 'OsPath' type.
module GHC.Paths.OsPath
  ( ghc
  , ghc_pkg
  , libdir
  , docdir
  ) where

import GHC.Paths qualified as GP
import GHC.Stack (HasCallStack)
import System.OsPath qualified as OP
import System.OsPath (OsPath)

ghc :: OsPath
ghc = unsafeEncodeUtf GP.ghc

ghc_pkg :: OsPath
ghc_pkg = unsafeEncodeUtf GP.ghc_pkg

libdir :: OsPath
libdir = unsafeEncodeUtf GP.libdir

docdir :: OsPath
docdir = unsafeEncodeUtf GP.docdir

-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: HasCallStack => String -> OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = either (error . show) id . OP.encodeUtf
#endif
