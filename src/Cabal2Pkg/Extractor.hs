{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor
  ( PackageMeta(..)
  , summariseCabal
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Component (ComponentMeta(..), extractComponents)
import Cabal2Pkg.Extractor.License (extractLicense)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Utils.ShortText (fromShortText)


data PackageMeta = PackageMeta
  { distName   :: !Text
  , categories :: ![Text]
  , maintainer :: !Text
  , comment    :: !Text
  , license    :: !Text
  , flags      :: !FlagMap
  , unrestrict :: !(Set Text)
  , components :: ![ComponentMeta]
  }
  deriving (Data, Show)


summariseCabal :: GenericPackageDescription -> CLI PackageMeta
summariseCabal gpd
  = do cat      <- CLI.category
       mtr      <- CLI.maintainer
       fs       <- CLI.pkgFlags
       (cs, ts) <- extractComponents gpd
       pure PackageMeta
         { distName   = T.pack . prettyShow . PD.package $ pd
         , categories = [cat]
         , maintainer = fromMaybe "pkgsrc-users@NetBSD.org" mtr
         , comment    = T.pack . fromShortText . PD.synopsis $ pd
         , license    = extractLicense pd
         , flags      = fs
         , unrestrict = ts
         , components = cs
         }
  where
    pd :: PackageDescription
    pd = GPD.packageDescription gpd
