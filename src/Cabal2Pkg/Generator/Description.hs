{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator.Description
  ( genDESCR
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Text.Wrap qualified as W


genDESCR :: PackageMeta -> TL.Text
genDESCR = TL.fromChunks . ((<> newline) <$>) . W.wrapTextToLines cfg width . description
  where
    cfg :: W.WrapSettings
    cfg = W.defaultWrapSettings

    width :: Int
    width = 80

    newline :: T.Text
    newline = "\n"
