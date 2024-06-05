{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Dependency
  ( DepSet(..), exeDeps, extLibDeps, libDeps, pkgConfDeps
  , extractDeps
  ) where

import Cabal2Pkg.CmdLine (CLI)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep, extractExeDep)
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep, extractExtLibDep)
import Cabal2Pkg.Extractor.Dependency.Library (LibDep, extractLibDep)
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep, extractPkgConfDep)
import Data.Data (Data)
import Distribution.Simple.BuildToolDepends qualified as BTD
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import GHC.Generics (Generic, Generically(..))
import UnliftIO.Async (Conc, conc, runConc)
import Lens.Micro.TH (makeLenses)


-- |A set of various kinds of dependencies, such as tool dependencies,
-- Haskell library dependencies, pkg-config dependencies, and external
-- library dependencies.
--
-- Note that it is tempting to use "Data.HashSet" instead of lists but we
-- should preserve the order the dependencies appear in @*.cabal@ files.
data DepSet
  = DepSet
    { _exeDeps     :: ![ExeDep]
    , _extLibDeps  :: ![ExtLibDep]
    , _libDeps     :: ![LibDep]
    , _pkgConfDeps :: ![PkgConfDep]
    }
  deriving (Data, Eq, Generic, Show)
  deriving (Monoid, Semigroup) via Generically DepSet

makeLenses ''DepSet


extractDeps :: C.PackageDescription -> C.BuildInfo -> CLI DepSet
extractDeps pkg bi
  = runConc $ DepSet <$> execs <*> extLibs <*> libs <*> pkgConfLibs
  where
    execs :: Conc CLI [ExeDep]
    execs = traverse (conc . extractExeDep)
            [ dep
            | dep <- BTD.getAllToolDependencies pkg bi
            , not (BTD.isInternal pkg dep)
            ]

    extLibs :: Conc CLI [ExtLibDep]
    extLibs = pure . (extractExtLibDep <$>) $ C.extraLibs bi

    libs :: Conc CLI [LibDep]
    libs = traverse (conc . extractLibDep)
           [ dep
           | dep <- C.targetBuildDepends bi
           , not (isInternalLib dep)
           ]
      where
        isInternalLib :: C.Dependency -> Bool
        isInternalLib = (C.pkgName (C.package pkg) ==) . C.depPkgName

    pkgConfLibs :: Conc CLI [PkgConfDep]
    pkgConfLibs = pure . (extractPkgConfDep <$>) $ C.pkgconfigDepends bi
