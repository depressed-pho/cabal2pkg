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
  ( Environment(..), CondBlock(..), always, extractCondBlock )
import Cabal2Pkg.Extractor.Dependency
  ( DepSet, isBuildable, extractDeps, extractSetupDeps )
import Cabal2Pkg.Extractor.Haddock (extractHaddock)
import Control.Monad (when, unless)
import Data.Data (Data)
import Data.Foldable (toList)
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
import Distribution.Types.SetupBuildInfo qualified as C
import Distribution.Types.UnqualComponentName qualified as C
import Lens.Micro.Platform ((^.), makeLenses)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import UnliftIO.Async (Conc, runConc)


data ComponentMeta = ComponentMeta
  { _cType :: !ComponentType
  , _cName :: !Text
  , _cDeps :: !(CondBlock DepSet)
  }
  deriving (Data, Eq, Show)

data ComponentType =
    -- |@custom-setup@ is technically not a component in Cabal's sense, but
    -- we treat it as such.
    CustomSetup
  | Library
  | ForeignLib
  | Executable

  deriving (Data, Eq, Show)

makeLenses ''ComponentMeta


extractComponents :: GenericPackageDescription -> CLI [ComponentMeta]
extractComponents gpd
  = do env     <- extractEnv
       cSetup  <- traverse  extractCSetup            (C.setupBuildInfo      pd)
       lib     <- traverse (extractLib    env      ) (GPD.condLibrary      gpd)
       subLibs <- traverse (extractLib    env . snd) (GPD.condSubLibraries gpd)
       frnLibs <- traverse (extractFrnLib env . snd) (GPD.condForeignLibs  gpd)
       execs   <- traverse (extractExe    env . snd) (GPD.condExecutables  gpd)
       pure . garbageCollect . mconcat $
         [ maybeToList cSetup
         , maybeToList lib
         , subLibs
         , frnLibs
         , execs
         ]
  where
    garbageCollect :: [ComponentMeta] -> [ComponentMeta]
    garbageCollect = filter p
      where
        -- Omit components that are never buildable. Conditionally
        -- buildable ones should be retained.
        p :: ComponentMeta -> Bool
        p = (^. cDeps . always . isBuildable)

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

    -- The custom-setup stanza does not support conditionals at all, which
    -- means we must do something special for that. See
    -- https://github.com/haskell/cabal/issues/4286
    extractCSetup :: C.SetupBuildInfo -> CLI ComponentMeta
    extractCSetup sbi =
      do deps <- runConc . extractSetupDeps $ sbi
         pure $ ComponentMeta
           { _cType = CustomSetup
           , _cName = "custom-setup"
           , _cDeps = CondBlock deps mempty
           }

    extractLib :: Environment -> C.CondTree C.ConfVar c C.Library -> CLI ComponentMeta
    extractLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Library -> Conc CLI DepSet
        extractContent = extractDeps pd . C.libBuildInfo

        extractOuter :: C.Library -> CondBlock DepSet -> ComponentMeta
        extractOuter lib deps =
          ComponentMeta
          { _cType = Library
          , _cName = case C.libName lib of
                       C.LMainLibName  -> T.pack . prettyShow . C.pkgName . C.package $ pd
                       C.LSubLibName n -> T.pack $ C.unUnqualComponentName n
          , _cDeps = deps
          }

    extractFrnLib :: Environment -> C.CondTree C.ConfVar c C.ForeignLib -> CLI ComponentMeta
    extractFrnLib = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.ForeignLib -> Conc CLI DepSet
        extractContent = extractDeps pd . C.foreignLibBuildInfo

        extractOuter :: C.ForeignLib -> CondBlock DepSet -> ComponentMeta
        extractOuter frnLib deps =
          ComponentMeta
          { _cType = ForeignLib
          , _cName = T.pack . C.unUnqualComponentName $ C.foreignLibName frnLib
          , _cDeps = deps
          }

    extractExe :: Environment -> C.CondTree C.ConfVar c C.Executable -> CLI ComponentMeta
    extractExe = extractCondBlock extractContent extractOuter
      where
        extractContent :: C.Executable -> Conc CLI DepSet
        extractContent = extractDeps pd . C.buildInfo

        extractOuter :: C.Executable -> CondBlock DepSet -> ComponentMeta
        extractOuter exe deps =
          ComponentMeta
          { _cType = Executable
          , _cName = T.pack . C.unUnqualComponentName $ C.exeName exe
          , _cDeps = deps
          }

-- |Extract package flags, manual or not, and combine them with what are
-- given via command-line options.
extractFlags :: FlagMap -> [C.PackageFlag] -> CLI FlagMap
extractFlags givenFlags flags =
  do let (defaulted, flags') = foldl' go (mempty, mempty) flags
         unknown             = M.keysSet $ M.difference givenFlags flags'
     showFlags <- CLI.showPkgFlags
     when showFlags $
       do unless (null defaulted) $
            CLI.info ( PP.nest 2 . PP.vsep $
                       [ PP.hsep ( [ "The package defines the following" ] <>
                                   ( if M.null givenFlags
                                     then mempty
                                     else ["additional"]
                                   ) <>
                                   [ "manual flags. You can override them with"
                                   , "-f or --flags options:"
                                   ] )
                       ] <>
                       [ PP.pretty '-' <+> pprPF pf
                       | pf <- defaulted
                       ]
                     )
          unless (S.null unknown) $
            CLI.warn ( PP.nest 2 . PP.vsep $
                       [ "Ignoring unknown flags:" ] <>
                       [ PP.pretty '-' <+> PP.pretty (C.unFlagName fn)
                       | fn <- toList unknown
                       ]
                     )
     pure flags'
  where
    pprPF :: C.PackageFlag -> Doc AnsiStyle
    pprPF pf =
      PP.nest 2 . PP.vsep $
      [ (PP.pretty . C.unFlagName . C.flagName $ pf) <> PP.colon
      , (extractHaddock . T.pack . C.flagDescription $ pf) <+>
        PP.parens ("default:" <+> PP.pretty (C.flagDefault pf))
      ]

    go :: ([C.PackageFlag], FlagMap) -> C.PackageFlag -> ([C.PackageFlag], FlagMap)
    go (defaulted, fa) pf =
      case M.lookup (C.flagName pf) givenFlags of
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
