{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor.Component
  ( ComponentMeta(..)
  , extractComponents
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Conditional
  ( Environment(..), CondBlock, extractCondBlock )
import Cabal2Pkg.Extractor.Dependency (Dependency, extractDependency)
import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Pretty (prettyShow)
import Distribution.Types.BuildInfo qualified as C
import Distribution.Types.CondTree qualified as C
import Distribution.Types.ConfVar qualified as C
import Distribution.Types.Flag qualified as C
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.Library qualified as C
import Distribution.Types.LibraryName qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import Distribution.Types.UnqualComponentName qualified as C
import UnliftIO.Async (mapConcurrently)


data ComponentMeta = ComponentMeta
  { type_        :: !ComponentType
  , name         :: !Text
  , dependencies :: !(CondBlock [Dependency])
  }
  deriving Show

data ComponentType
  = Library
  | Executable
  deriving Show


extractComponents :: GenericPackageDescription -> CLI [ComponentMeta]
extractComponents gpd
  = do env  <- extractEnv
       lib  <- traverse (extractLib env) (GPD.condLibrary gpd)
       -- FIXME: also executables
       pure $ concat [ maybeToList lib
                     ]
  where
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

    extractLib :: Environment -> C.CondTree C.ConfVar c C.Library -> CLI ComponentMeta
    extractLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Library -> CLI [Dependency]
        extractContent
          = (catMaybes <$>)
          . mapConcurrently extractDependency
          . C.targetBuildDepends
          . C.libBuildInfo

        extractOuter :: Applicative f => C.Library -> CondBlock [Dependency] -> f ComponentMeta
        extractOuter lib deps
          = pure ComponentMeta
            { type_        = Library
            , name         = case C.libName lib of
                               C.LMainLibName  -> T.pack . prettyShow . C.pkgName . C.package $ pd
                               C.LSubLibName n -> T.pack $ C.unUnqualComponentName n
              -- FIXME: buildDeps
            , dependencies = deps
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
