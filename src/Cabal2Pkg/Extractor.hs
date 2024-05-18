{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor
  ( PackageMeta(..)
  , summariseCabal
  ) where

import Cabal2Pkg.Extractor.License (extractLicense)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Utils.ShortText (fromShortText)
import GHC.Generics (Generic, Generically(..))


data PackageMeta = PackageMeta
  { distName   :: Text
  , categories :: [Text]
  , maintainer :: Text
  , comment    :: Text
  , license    :: Text
  }
  deriving (Generic, Show)
  deriving ToJSON via Generically PackageMeta


summariseCabal :: Text -> Maybe Text -> GenericPackageDescription -> PackageMeta
summariseCabal category maintainer gpd
  = PackageMeta
    { distName   = T.pack . prettyShow . PD.package $ pd
    , categories = [category]
    , maintainer = fromMaybe "pkgsrc-users@NetBSD.org" maintainer
    , comment    = T.pack . fromShortText . PD.synopsis $ pd
    , license    = extractLicense pd
    }
  where
    pd :: PackageDescription
    pd = GPD.packageDescription gpd