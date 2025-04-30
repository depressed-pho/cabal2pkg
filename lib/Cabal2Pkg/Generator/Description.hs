{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator.Description
  ( genDESCR
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Extractor.Haddock (extractHaddock)
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Prettyprinter (LayoutOptions(..), PageWidth(..))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text (renderLazy)

genDESCR :: HasCallStack => Natural -> PackageMeta -> TL.Text
genDESCR width = eof
                 . renderLazy
                 . PP.layoutPretty opts
                 . extractHaddock
                 . description
  where
    opts :: LayoutOptions
    opts = LayoutOptions
           { layoutPageWidth = AvailablePerLine (fromIntegral width) 1
           }

    eof :: TL.Text -> TL.Text
    eof txt
      | TL.null txt = txt
      | otherwise   = txt <> "\n"
