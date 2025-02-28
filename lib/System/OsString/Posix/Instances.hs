{-# OPTIONS_GHC "-Wno-orphans" #-}
-- |Orphan instances for 'PosixString'
module System.OsString.Posix.Instances () where

import Data.CaseInsensitive (FoldCase(..))
import System.OsString.Data.ByteString.Short (fromShort, toShort)
import System.OsString.Internal.Types (PosixString(..))

instance FoldCase PosixString where
  foldCase :: PosixString -> PosixString
  foldCase = PosixString . toShort . foldCase . fromShort . getPosixString
