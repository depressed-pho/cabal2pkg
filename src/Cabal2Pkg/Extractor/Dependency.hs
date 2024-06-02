{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency
  ( DepSet(..)
  , extractDeps
  ) where

import Cabal2Pkg.CmdLine (CLI)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep, extractExeDep)
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep, extractExtLibDep)
import Cabal2Pkg.Extractor.Dependency.Library (LibDep, extractLibDep)
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep, extractPkgConfDep)
import Data.Maybe (catMaybes)
import Distribution.Simple.BuildToolDepends qualified as BTD
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import GHC.Generics (Generic, Generically(..))
import UnliftIO.Async (Conc, conc, runConc)


-- |A set of various kinds of dependencies, such as tool dependencies,
-- Haskell library dependencies, pkg-config dependencies, and external
-- library dependencies.
--
-- Note that it is tempting to use "Data.HashSet" instead of lists but we
-- should preserve the order the dependencies appear in @*.cabal@ files.
data DepSet
  = DepSet
    { exeDeps     :: ![ExeDep]
    , extLibDeps  :: ![ExtLibDep]
    , libDeps     :: ![LibDep]
    , pkgConfDeps :: ![PkgConfDep]
    }
  deriving (Eq, Generic, Show)
  deriving (Monoid, Semigroup) via Generically DepSet

extractDeps :: C.PackageDescription -> C.BuildInfo -> CLI DepSet
extractDeps pkg bi@(C.BuildInfo {..})
  = runConc $ DepSet <$> execs <*> extLibs <*> libs <*> pkgConfLibs
  where
    execs :: Conc CLI [ExeDep]
    execs = catMaybes
            <$> traverse (conc . extractExeDep)
                [ dep
                | dep <- BTD.getAllToolDependencies pkg bi
                , not (BTD.isInternal pkg dep)
                ]

    extLibs :: Conc CLI [ExtLibDep]
    extLibs = pure . (extractExtLibDep <$>) $ extraLibs

    libs :: Conc CLI [LibDep]
    libs = catMaybes
           <$> traverse (conc . extractLibDep)
               [ dep
               | dep <- targetBuildDepends
               , not (isInternalLib dep)
               ]
      where
        isInternalLib :: C.Dependency -> Bool
        isInternalLib = (C.pkgName (C.package pkg) ==) . C.depPkgName

    pkgConfLibs :: Conc CLI [PkgConfDep]
    pkgConfLibs = pure . (extractPkgConfDep <$>) $ pkgconfigDepends
