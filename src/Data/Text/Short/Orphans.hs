{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Text.Short.Orphans () where

import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Data.CaseInsensitive (FoldCase(..))

instance FoldCase ShortText where
  foldCase :: ShortText -> ShortText
  -- NOTE: Maybe this isn't the most efficient way to do it but I cannot
  -- find any better ways.
  foldCase = TS.fromText . T.toCaseFold . TS.toText
