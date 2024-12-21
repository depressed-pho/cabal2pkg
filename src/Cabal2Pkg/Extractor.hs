{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor
  ( PackageMeta(..)
  , summariseCabal
  , omitHackageDefaults
  , fillInMasterSites
  , hasLibraries
  , hasForeignLibs
  , hasExecutables
  ) where

import Cabal2Pkg.CmdLine (CLI, FlagMap)
import Cabal2Pkg.CmdLine qualified as CLI
import Cabal2Pkg.Extractor.Component
  ( ComponentMeta(..), ComponentType(..), cType, extractComponents )
import Cabal2Pkg.Extractor.License (extractLicense)
import Cabal2Pkg.PackageURI (PackageURI(HTTP))
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
import Distribution.Utils.ShortText qualified as ST
import GHC.Stack (HasCallStack)
import Lens.Micro ((^.))
import Network.URI (URI(uriPath), uriToString)
import System.FilePath.Posix qualified as FP


data PackageMeta = PackageMeta
  { distBase    :: !Text
  , distVersion :: !Version
  , pkgBase     :: !Text
  , pkgPath     :: !Text
  , categories  :: ![Text]
    -- ^@[]@ means it's in Hackage
  , masterSites :: ![Text]
  , maintainer  :: !Text
    -- ^@Nothing@ means it's in Hackage
  , homepage    :: !(Maybe Text)
  , comment     :: !Text
  , description :: !Text
  , license     :: !Text
  , flags       :: !FlagMap
  , unrestrict  :: !(Set Text)
  , components  :: ![ComponentMeta]
  }
  deriving (Data, Show)

-- |Construct a 'PackageMeta' from a given package description. This
-- function does not take account of the current state of the PKGPATH, that
-- is, it doesn't read its @Makefile@ even if it exists.
summariseCabal :: GenericPackageDescription -> CLI PackageMeta
summariseCabal gpd
  = do path     <- CLI.pkgPath
       base     <- CLI.pkgBase
       cat      <- CLI.pkgCategory
       mtr      <- CLI.maintainer
       fs       <- CLI.pkgFlags
       (cs, ts) <- extractComponents gpd
       pure PackageMeta
         { distBase    = T.pack . C.unPackageName . C.pkgName . PD.package $ pd
         , distVersion = C.pkgVersion . PD.package $ pd
         , pkgBase     = base
         , pkgPath     = path
         , categories  = [cat]
         , masterSites = []
         , maintainer  = fromMaybe "pkgsrc-users@NetBSD.org" mtr
         , homepage    = Just . T.pack . ST.fromShortText . PD.homepage $ pd
         , comment     = T.pack . ST.fromShortText . PD.synopsis $ pd
         , description = extractDescription pd
         , license     = extractLicense pd
         , flags       = fs
         , unrestrict  = ts
         , components  = cs
         }
  where
    pd :: PackageDescription
    pd = GPD.packageDescription gpd

extractDescription :: PackageDescription -> Text
extractDescription pd =
  let descr = PD.description pd
      synop = PD.synopsis pd
  in
    if ST.null descr then
      if ST.null synop then
        T.intercalate "\n"
          [ "TODO: Fill in a short description of the package."
          , "TODO: It should fit on a traditional terminal of 80x25 characters."
          ]
      else
        T.pack . ST.fromShortText $ synop
    else
      T.pack . ST.fromShortText $ descr

omitHackageDefaults :: PackageMeta -> PackageMeta
omitHackageDefaults pm =
  pm { masterSites = []
     , homepage    = Nothing
     }

fillInMasterSites :: HasCallStack => PackageURI -> PackageMeta -> PackageMeta
fillInMasterSites uri pm =
  case uri of
    HTTP httpURI ->
      let path' = FP.dropFileName . uriPath $ httpURI
          uri'  = httpURI { uriPath = path' }
      in
        pm { masterSites = pure . T.pack $ uriToString id uri' "" }
    _ ->
      error ("Cannot fill in MASTER_SITES for URI: " <> show uri)

hasLibraries :: PackageMeta -> Bool
hasLibraries
  = any ((== Library) . (^. cType)) . components

hasForeignLibs :: PackageMeta -> Bool
hasForeignLibs
  = any ((== ForeignLib) . (^. cType)) . components

hasExecutables :: PackageMeta -> Bool
hasExecutables
  = any ((== Executable) . (^. cType)) . components
