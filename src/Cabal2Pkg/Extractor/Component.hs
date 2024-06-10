{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Component
  ( ComponentMeta(..), cType, cName, cDeps
  , ComponentType(..)
  , extractComponents
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Conditional
  ( Environment(..), CondBlock, extractCondBlock )
import Cabal2Pkg.Extractor.Dependency (DepSet, extractDeps)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree qualified as C
import Distribution.Types.ConfVar qualified as C
import Distribution.Types.Executable qualified as C
import Distribution.Types.ForeignLib qualified as C
import Distribution.Types.Flag qualified as C
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.Library qualified as C
import Distribution.Types.LibraryName qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import Distribution.Types.UnqualComponentName qualified as C
import Lens.Micro ((^.), to)
import Lens.Micro.TH (makeLenses)
import UnliftIO.Async (Conc)


data ComponentMeta = ComponentMeta
  { _cType :: !ComponentType
  , _cName :: !Text
  , _cDeps :: !(CondBlock DepSet)
  }
  deriving (Data, Eq, Show)

data ComponentType
  = Library
  | ForeignLib
  | Executable
  deriving (Data, Eq, Show)

makeLenses ''ComponentMeta


extractComponents :: GenericPackageDescription -> CLI ([ComponentMeta], Set Text)
extractComponents gpd
  = do env     <- extractEnv
       lib     <- aggregateEach <$> traverse (extractLib    env) (GPD.condLibrary gpd)
       subLibs <- aggregateEach <$> traverse (extractLib    env) (snd <$> GPD.condSubLibraries gpd)
       frnLibs <- aggregateEach <$> traverse (extractFrnLib env) (snd <$> GPD.condForeignLibs  gpd)
       execs   <- aggregateEach <$> traverse (extractExe    env) (snd <$> GPD.condExecutables  gpd)
       pure $ aggregateAll
         [ first maybeToList lib
         , subLibs
         , frnLibs
         , execs
         ]
  where
    -- Why do we keep track of Set Text manually, and don't use WriterT?
    -- Because we need parallelisation which restricts our monad to
    -- MonadUnliftIO.
    aggregateAll :: [ ([ComponentMeta], Set Text) ] -> ([ComponentMeta], Set Text)
    aggregateAll = first (filter (not . isEmpty)) . mconcat

    isEmpty :: ComponentMeta -> Bool
    isEmpty c = c ^. cDeps . to (== mempty)

    aggregateEach :: Traversable t
                  => t (ComponentMeta, Set Text)
                  -> (t ComponentMeta, Set Text)
    aggregateEach = swap . mapAccumL go mempty
      where
        go ts0 (c, ts) = (ts0 <> ts, c)

    extractEnv :: CLI Environment
    extractEnv
      = do givenFlags <- CLI.pkgFlags
           flags      <- extractFlags givenFlags (GPD.genPackageFlags gpd)
           ghcVer     <- CLI.ghcVersion
           pure Environment
             { flags      = flags
             , ghcVersion = ghcVer
             }

    pd :: C.PackageDescription
    pd = GPD.packageDescription gpd

    aggregateOuter :: (CondBlock DepSet -> ComponentMeta)
                   -> CondBlock (DepSet, Set Text)
                   -> (ComponentMeta, Set Text)
    aggregateOuter f = first f . swap . mapAccumL go mempty
      where
        go ts0 (deps, ts) = (ts0 <> ts, deps)

    extractLib :: Environment -> C.CondTree C.ConfVar c C.Library -> CLI (ComponentMeta, Set Text)
    extractLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Library -> Conc CLI (DepSet, Set Text)
        extractContent = extractDeps pd . C.libBuildInfo

        extractOuter :: C.Library
                     -> CondBlock (DepSet, Set Text)
                     -> (ComponentMeta, Set Text)
        extractOuter lib = aggregateOuter go
          where
            go deps
              = ComponentMeta
                { _cType = Library
                , _cName = case C.libName lib of
                             C.LMainLibName  -> T.pack . prettyShow . C.pkgName . C.package $ pd
                             C.LSubLibName n -> T.pack $ C.unUnqualComponentName n
                , _cDeps = deps
                }

    extractFrnLib :: Environment -> C.CondTree C.ConfVar c C.ForeignLib -> CLI (ComponentMeta, Set Text)
    extractFrnLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.ForeignLib -> Conc CLI (DepSet, Set Text)
        extractContent = extractDeps pd . C.foreignLibBuildInfo

        extractOuter :: C.ForeignLib
                     -> CondBlock (DepSet, Set Text)
                     -> (ComponentMeta, Set Text)
        extractOuter frnLib = aggregateOuter go
          where
            go deps
              = ComponentMeta
                { _cType = ForeignLib
                , _cName = T.pack . C.unUnqualComponentName $ C.foreignLibName frnLib
                , _cDeps = deps
                }

    extractExe :: Environment -> C.CondTree C.ConfVar c C.Executable -> CLI (ComponentMeta, Set Text)
    extractExe = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Executable -> Conc CLI (DepSet, Set Text)
        extractContent = extractDeps pd . C.buildInfo

        extractOuter :: C.Executable
                     -> CondBlock (DepSet, Set Text)
                     -> (ComponentMeta, Set Text)
        extractOuter exe = aggregateOuter go
          where
            go deps
              = ComponentMeta
                { _cType = Executable
                , _cName = T.pack . C.unUnqualComponentName $ C.exeName exe
                , _cDeps = deps
                }


extractFlags :: FlagMap -> [C.PackageFlag] -> CLI FlagMap
extractFlags givenFlags flags
  = do let (defaulted, flags') = foldl' go (mempty, mempty) flags
           unknown             = M.keysSet $ M.difference givenFlags flags'
       unless (null defaulted) $
         do CLI.info ("The package defines the following manual flags. You " <>
                      "can override them with `-f' or `--flags' options:")
            mapM_ (CLI.info . T.pack . ("  - " <>) . showPF) defaulted
       unless (S.null unknown) $
         do CLI.warn "Ignoring unknown flags:"
            mapM_ (CLI.warn . T.pack . ("  - " <>) . C.unFlagName) unknown
       pure flags'
  where
    showPF :: C.PackageFlag -> String
    showPF pf
      = (C.unFlagName . C.flagName $ pf) <> ": " <> show (C.flagDefault pf)

    go :: ([C.PackageFlag], FlagMap) -> C.PackageFlag -> ([C.PackageFlag], FlagMap)
    go (defaulted, fa) pf
      = case M.lookup (C.flagName pf) givenFlags of
          Just b ->
            ( defaulted
            , M.insert (C.flagName pf) b fa
            )
          Nothing
            | C.flagManual pf ->
                ( defaulted <> [pf]
                , M.insert (C.flagName pf) (C.flagDefault pf) fa
                )
            | otherwise ->
                ( defaulted
                , M.insert (C.flagName pf) (C.flagDefault pf) fa
                )
