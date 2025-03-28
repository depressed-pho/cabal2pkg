{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Dependency
  ( DepSet(..), isBuildable, exeDeps, extLibDeps, libDeps, pkgConfDeps
  , extractDeps
  , extractSetupDeps
  ) where

import Cabal2Pkg.CmdLine (CLI)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep, extractExeDep)
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep, extractExtLibDep)
import Cabal2Pkg.Extractor.Dependency.Library (LibDep, extractLibDep)
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep, extractPkgConfDep)
import Data.Data (Data)
import Data.IsNull (IsNull(..))
import Data.Set.Ordered (OSet, Bias(..), L)
import Data.Set.Ordered qualified as OS
import Distribution.Simple.BuildToolDepends qualified as BTD
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import Distribution.Types.SetupBuildInfo qualified as C
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

instance IsNull DepSet where
  isNull ds =
    OS.null (ds ^. exeDeps    ) &&
    OS.null (ds ^. extLibDeps ) &&
    OS.null (ds ^. libDeps    ) &&
    OS.null (ds ^. pkgConfDeps)

extractDeps :: C.PackageDescription -> C.BuildInfo -> Conc CLI DepSet
extractDeps pd bi =
  DepSet (C.buildable bi) <$> execs <*> extLibs <*> libs <*> pkgConfLibs
  where
    execs :: Conc CLI (OSet ExeDep)
    execs = OS.fromList <$> traverse (conc . extractExeDep)
            [ dep
            | dep <- BTD.getAllToolDependencies pd bi
            , not (BTD.isInternal pd dep)
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
        isInternalLib = (C.pkgName (C.package pd) ==) . C.depPkgName

    pkgConfLibs :: Conc CLI (OSet PkgConfDep)
    pkgConfLibs = pure . OS.fromList . (extractPkgConfDep <$>) $ C.pkgconfigDepends bi

extractSetupDeps :: C.SetupBuildInfo -> Conc CLI DepSet
extractSetupDeps sbi =
  -- 'Conc' is not a monad. This is an ApplicativeDo.
  do deps <- libs
     pure $ DepSet True OS.empty OS.empty deps OS.empty
  where
    -- Custom setups can never depend on this very package because that
    -- forms a cyclic dependency, which means we don't need to worry about
    -- internal dependencies.
    libs :: Conc CLI (OSet LibDep)
    libs = OS.fromList <$> traverse (conc . extractLibDep) (C.setupDepends sbi)
