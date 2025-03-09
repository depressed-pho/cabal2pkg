{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Dependency
  ( DepSet(..), isBuildable, exeDeps, extLibDeps, libDeps, pkgConfDeps
  , extractDeps
  ) where

import Cabal2Pkg.CmdLine (CLI)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep, extractExeDep)
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep, extractExtLibDep)
import Cabal2Pkg.Extractor.Dependency.Library (LibDep, extractLibDep)
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep, extractPkgConfDep)
import Data.Data (Data)
import Data.Set.Ordered (OSet, Bias(..), L)
import Data.Set.Ordered qualified as OS
import Distribution.Simple.BuildToolDepends qualified as BTD
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import GHC.Generics (Generic)
import UnliftIO.Async (Conc, conc)
import Lens.Micro.Platform ((^.), makeLenses)


-- |A set of various kinds of dependencies, such as tool dependencies,
-- Haskell library dependencies, pkg-config dependencies, and external
-- library dependencies. It preserves the order of dependencies apear in
-- package metadata. It also has a boolean flag indicating whether the
-- component is buildable or not, which has nothing to do with
-- dependencies, but has nowhere else to go.
data DepSet
  = DepSet
    { _isBuildable :: !Bool
    , _exeDeps     :: !(OSet ExeDep)
    , _extLibDeps  :: !(OSet ExtLibDep)
    , _libDeps     :: !(OSet LibDep)
    , _pkgConfDeps :: !(OSet PkgConfDep)
    }
  deriving (Data, Eq, Generic, Show)

makeLenses ''DepSet

instance Semigroup DepSet where
  a <> b =
    DepSet { _isBuildable = a^.isBuildable && b^.isBuildable
           , _exeDeps     = OS.unbiased $ Bias @L (a^.exeDeps    ) <> Bias @L (b^.exeDeps)
           , _extLibDeps  = OS.unbiased $ Bias @L (a^.extLibDeps ) <> Bias @L (b^.extLibDeps)
           , _libDeps     = OS.unbiased $ Bias @L (a^.libDeps    ) <> Bias @L (b^.libDeps)
           , _pkgConfDeps = OS.unbiased $ Bias @L (a^.pkgConfDeps) <> Bias @L (b^.pkgConfDeps)
           }

instance Monoid DepSet where
  mempty =
    DepSet { _isBuildable = True
           , _exeDeps     = OS.empty
           , _extLibDeps  = OS.empty
           , _libDeps     = OS.empty
           , _pkgConfDeps = OS.empty
           }

extractDeps :: C.PackageDescription -> C.BuildInfo -> Conc CLI DepSet
extractDeps pkg bi
  = DepSet (C.buildable bi) <$> execs <*> extLibs <*> libs <*> pkgConfLibs
  where
    execs :: Conc CLI (OSet ExeDep)
    execs = OS.fromList <$> traverse (conc . extractExeDep)
            [ dep
            | dep <- BTD.getAllToolDependencies pkg bi
            , not (BTD.isInternal pkg dep)
            ]

    extLibs :: Conc CLI (OSet ExtLibDep)
    extLibs = pure . OS.fromList . (extractExtLibDep <$>) $ C.extraLibs bi

    libs :: Conc CLI (OSet LibDep)
    libs = OS.fromList <$> traverse (conc . extractLibDep)
           [ dep
           | dep <- C.targetBuildDepends bi
           , not (isInternalLib dep)
           ]
      where
        -- It's an internal dependency if a package depends on itself.
        isInternalLib :: C.Dependency -> Bool
        isInternalLib = (C.pkgName (C.package pkg) ==) . C.depPkgName

    pkgConfLibs :: Conc CLI (OSet PkgConfDep)
    pkgConfLibs = pure . OS.fromList . (extractPkgConfDep <$>) $ C.pkgconfigDepends bi
