{-# LANGUAGE DeriveAnyClass #-} -- for deriving Hashable
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency.ExternalLib
  ( ExtLibDep(..)
  , extractExtLibDep
  ) where

import Data.Hashable (Hashable(..))
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import GHC.Generics (Generic)


-- |Dependency on a non-Haskell library. In pkgsrc we declare these
-- dependencies by including a corresponding @buildlink3.mk@ but it's
-- impossible to automatically infer the path to it.
data ExtLibDep
  = ExtLibDep
    { -- |The name of an external library, such as @"z"@ for @libz@.
      name :: !ShortText
    }
  deriving (Eq, Generic, Hashable, Show)


extractExtLibDep :: String -> ExtLibDep
extractExtLibDep = ExtLibDep . TS.fromString
