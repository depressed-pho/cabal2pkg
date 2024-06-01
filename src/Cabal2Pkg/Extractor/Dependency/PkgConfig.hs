{-# LANGUAGE DeriveAnyClass #-} -- for deriving Hashable
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency.PkgConfig
  ( PkgConfDep(..)
  , extractPkgConfDep
  ) where

import Data.Hashable (Hashable(..))
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Distribution.Types.PkgconfigDependency qualified as C
import Distribution.Types.PkgconfigName qualified as C
import GHC.Generics (Generic)


-- |Dependency on an external pkg-config library. In pkgsrc we declare
-- these dependencies by including a corresponding @buildlink3.mk@ but it's
-- impossible to automatically infer the path to it.
data PkgConfDep
  = PkgConfDep
    { -- |The name of a pkg-config package, such as @"cups"@.
      name :: !ShortText
    }
  deriving (Eq, Generic, Hashable, Show)


extractPkgConfDep :: C.PkgconfigDependency -> PkgConfDep
extractPkgConfDep (C.PkgconfigDependency pkgName _)
  = PkgConfDep . TS.fromString . C.unPkgconfigName $ pkgName
