{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Component
  ( ComponentMeta(..)
  , extractComponents
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Conditional
  ( Environment(..), CondBlock, extractCondBlock )
import Cabal2Pkg.Extractor.Dependency (DepSet, extractDeps)
import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
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


data ComponentMeta = ComponentMeta
  { type_ :: !ComponentType
  , name  :: !Text
  , deps  :: !(CondBlock DepSet)
  }
  deriving Show

data ComponentType
  = Library
  | ForeignLib
  | Executable
  deriving Show


extractComponents :: GenericPackageDescription -> CLI [ComponentMeta]
extractComponents gpd
  = do env     <- extractEnv
       lib     <- traverse (extractLib    env) (GPD.condLibrary gpd)
       subLibs <- traverse (extractLib    env) (snd <$> GPD.condSubLibraries gpd)
       frnLibs <- traverse (extractFrnLib env) (snd <$> GPD.condForeignLibs  gpd)
       execs   <- traverse (extractExe    env) (snd <$> GPD.condExecutables  gpd)
       pure . filter (not . isEmpty) $ concat [ maybeToList lib
                                              , subLibs
                                              , frnLibs
                                              , execs
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

    isEmpty :: ComponentMeta -> Bool
    isEmpty (ComponentMeta {..}) = deps == mempty

    extractLib :: Environment -> C.CondTree C.ConfVar c C.Library -> CLI ComponentMeta
    extractLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Library -> CLI DepSet
        extractContent = extractDeps pd . C.libBuildInfo

        extractOuter :: Applicative f => C.Library -> CondBlock DepSet -> f ComponentMeta
        extractOuter lib deps
          = pure ComponentMeta
            { type_ = Library
            , name  = case C.libName lib of
                        C.LMainLibName  -> T.pack . prettyShow . C.pkgName . C.package $ pd
                        C.LSubLibName n -> T.pack $ C.unUnqualComponentName n
            , deps  = deps
            }

    extractFrnLib :: Environment -> C.CondTree C.ConfVar c C.ForeignLib -> CLI ComponentMeta
    extractFrnLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.ForeignLib -> CLI DepSet
        extractContent = extractDeps pd . C.foreignLibBuildInfo

        extractOuter :: Applicative f => C.ForeignLib -> CondBlock DepSet -> f ComponentMeta
        extractOuter frnLib deps
          = pure ComponentMeta
            { type_ = ForeignLib
            , name  = T.pack . C.unUnqualComponentName $ C.foreignLibName frnLib
            , deps  = deps
            }

    extractExe :: Environment -> C.CondTree C.ConfVar c C.Executable -> CLI ComponentMeta
    extractExe = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Executable -> CLI DepSet
        extractContent = extractDeps pd . C.buildInfo

        extractOuter :: Applicative f => C.Executable -> CondBlock DepSet -> f ComponentMeta
        extractOuter exe deps
          = pure ComponentMeta
            { type_ = Executable
            , name  = T.pack . C.unUnqualComponentName $ C.exeName exe
            , deps  = deps
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
