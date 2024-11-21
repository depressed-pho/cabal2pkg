{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor
  ( PackageMeta(..)
  , summariseCabal
  , hasLibraries
  , hasForeignLibs
  , hasExecutables
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Component
  ( ComponentMeta(..), ComponentType(..), cType, extractComponents )
import Cabal2Pkg.Extractor.License (extractLicense)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Types.PackageId qualified as C
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Utils.ShortText (fromShortText)
import Lens.Micro ((^.))
import System.OsPath (OsPath)
import System.OsPath qualified as OP


data PackageMeta = PackageMeta
  { distBase    :: !Text
  , distVersion :: !Version
  , pkgBase     :: !Text
  , pkgPath     :: !Text
  , categories  :: ![Text]
  , maintainer  :: !Text
  , comment     :: !Text
  , license     :: !Text
  , flags       :: !FlagMap
  , unrestrict  :: !(Set Text)
  , components  :: ![ComponentMeta]
  }
  deriving (Data, Show)


summariseCabal :: GenericPackageDescription -> CLI PackageMeta
summariseCabal gpd
  = do path     <- T.pack <$> (OP.decodeUtf . takeCatAndName  =<< CLI.pkgPath)
       base     <- T.pack <$> (OP.decodeUtf . OP.takeFileName =<< CLI.pkgPath)
       cat      <- CLI.category
       mtr      <- CLI.maintainer
       fs       <- CLI.pkgFlags
       (cs, ts) <- extractComponents gpd
       pure PackageMeta
         { distBase    = T.pack . C.unPackageName . C.pkgName . PD.package $ pd
         , distVersion = C.pkgVersion . PD.package $ pd
         , pkgBase     = base
         , pkgPath     = path
         , categories  = [cat]
         , maintainer  = fromMaybe "pkgsrc-users@NetBSD.org" mtr
         , comment     = T.pack . fromShortText . PD.synopsis $ pd
         , license     = extractLicense pd
         , flags       = fs
         , unrestrict  = ts
         , components  = cs
         }
  where
    pd :: PackageDescription
    pd = GPD.packageDescription gpd

    takeCatAndName :: OsPath -> OsPath
    takeCatAndName = OP.joinPath . reverse . take 2 . reverse . OP.splitPath

hasLibraries :: PackageMeta -> Bool
hasLibraries
  = any ((== Library) . (^. cType)) . components

hasForeignLibs :: PackageMeta -> Bool
hasForeignLibs
  = any ((== ForeignLib) . (^. cType)) . components

hasExecutables :: PackageMeta -> Bool
hasExecutables
  = any ((== Executable) . (^. cType)) . components
