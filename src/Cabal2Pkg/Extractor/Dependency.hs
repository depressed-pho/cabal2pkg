{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency
  ( DepSet(..)
  , extractDeps
  ) where

import Cabal2Pkg.CmdLine (CLI)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep, extractExeDep)
import Cabal2Pkg.Extractor.Dependency.Library (LibDep, extractLibDep)
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep, extractPkgConfDep)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (catMaybes)
import Distribution.Simple.BuildToolDepends qualified as BTD
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.PackageDescription qualified as C
import GHC.Generics (Generic, Generically(..))
import UnliftIO.Async (Conc, conc, runConc)


-- |A set of various kinds of dependencies, such as tool dependencies,
-- Haskell library dependencies, pkg-config dependencies, and external
-- library dependencies.
data DepSet
  = DepSet
    { exeDeps     :: !(HashSet ExeDep)
    , libDeps     :: !(HashSet LibDep)
    , pkgConfDeps :: !(HashSet PkgConfDep)
    }
  deriving (Eq, Generic, Show)
  deriving (Monoid, Semigroup) via Generically DepSet

extractDeps :: C.PackageDescription -> C.BuildInfo -> CLI DepSet
extractDeps pkg bi@(C.BuildInfo {..})
  = runConc $ DepSet <$> execs <*> libs <*> pkgConfLibs
  where
    execs :: Conc CLI (HashSet ExeDep)
    execs = HS.fromList
            <$> traverse (conc . extractExeDep)
                [ dep
                | dep <- BTD.getAllToolDependencies pkg bi
                , not (BTD.isInternal pkg dep)
                ]

    libs :: Conc CLI (HashSet LibDep)
    libs = HS.fromList . catMaybes
           <$> traverse (conc . extractLibDep) targetBuildDepends

    pkgConfLibs :: Conc CLI (HashSet PkgConfDep)
    pkgConfLibs = pure . HS.fromList . (extractPkgConfDep <$>) $  pkgconfigDepends
