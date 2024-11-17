{-# LANGUAGE DerivingStrategies #-}
module Cabal2Pkg.Extractor.Dependency.PkgConfig
  ( PkgConfDep(..)
  , extractPkgConfDep
  ) where

import Data.Data (Data)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.PkgconfigDependency qualified as C
import Distribution.Types.PkgconfigName qualified as C


-- |Dependency on an external pkg-config library. In pkgsrc we declare
-- these dependencies by including a corresponding @buildlink3.mk@ but it's
-- impossible to automatically infer the path to it.
newtype PkgConfDep
  = PkgConfDep
    { -- |The name of a pkg-config package, such as @"cups"@.
      name :: Text
    }
  deriving stock (Data, Eq, Show)


extractPkgConfDep :: C.PkgconfigDependency -> PkgConfDep
extractPkgConfDep (C.PkgconfigDependency pkgName _)
  = PkgConfDep . T.pack . C.unPkgconfigName $ pkgName
