{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module Cabal2Pkg.Utils
  (
  ) where

import Data.String (IsString(..))
import GHC.Stack (HasCallStack)
import System.OsPath qualified as OP
import System.OsPath (OsPath)


-- |An orphan instance of IsString for OsPath. No idea why the upstream
-- doesn't provide this.
instance IsString OsPath where
  fromString :: String -> OsPath
  fromString = unsafeEncodeUtf


-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: HasCallStack => String -> OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = either (error . show) id . OP.encodeUtf
#endif
