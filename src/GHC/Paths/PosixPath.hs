{-# LANGUAGE CPP #-}
-- |This is an alternative API for ghc-paths based on the 'PosixPath' type.
module GHC.Paths.PosixPath
  ( ghc
  , ghcPkg
  , libDir
  , docDir
  ) where

import GHC.Paths qualified as GP
import GHC.Stack (HasCallStack)
import System.OsPath.Posix (PosixPath)
import System.OsPath.Posix qualified as OP

ghc :: PosixPath
ghc = unsafeEncodeUtf GP.ghc

ghcPkg :: PosixPath
ghcPkg = unsafeEncodeUtf GP.ghc_pkg

libDir :: PosixPath
libDir = unsafeEncodeUtf GP.libdir

docDir :: PosixPath
docDir = unsafeEncodeUtf GP.docdir

-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: HasCallStack => String -> PosixPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = either (error . show) id . OP.encodeUtf
#endif
