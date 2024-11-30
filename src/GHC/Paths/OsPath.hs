{-# LANGUAGE CPP #-}
-- |This is an alternative API for ghc-paths based on the 'OsPath' type.
module GHC.Paths.OsPath
  ( ghc
  , ghcPkg
  , libDir
  , docDir
  ) where

import GHC.Paths qualified as GP
import GHC.Stack (HasCallStack)
import System.OsPath qualified as OP
import System.OsPath (OsPath)

ghc :: OsPath
ghc = unsafeEncodeUtf GP.ghc

ghcPkg :: OsPath
ghcPkg = unsafeEncodeUtf GP.ghc_pkg

libDir :: OsPath
libDir = unsafeEncodeUtf GP.libdir

docDir :: OsPath
docDir = unsafeEncodeUtf GP.docdir

-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: HasCallStack => String -> OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = either (error . show) id . OP.encodeUtf
#endif
