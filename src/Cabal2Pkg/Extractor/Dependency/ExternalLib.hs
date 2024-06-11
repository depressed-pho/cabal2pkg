module Cabal2Pkg.Extractor.Dependency.ExternalLib
  ( ExtLibDep(..)
  , extractExtLibDep
  ) where

import Data.Data (Data)
import Data.Text (Text)
import Data.Text qualified as T


-- |Dependency on a non-Haskell library. In pkgsrc we declare these
-- dependencies by including a corresponding @buildlink3.mk@ but it's
-- impossible to automatically infer the path to it.
newtype ExtLibDep
  = ExtLibDep
    { -- |The name of an external library, such as @"z"@ for @libz@.
      name :: Text
    }
  deriving (Data, Eq, Show)


extractExtLibDep :: String -> ExtLibDep
extractExtLibDep = ExtLibDep . T.pack
