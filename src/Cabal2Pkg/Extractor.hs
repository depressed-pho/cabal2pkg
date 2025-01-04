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
import Cabal2Pkg.RawMeta (RawMeta(..))
import Cabal2Pkg.Site (PackageURI)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Types.PackageId qualified as C
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Distribution.Utils.ShortText qualified as ST
import Lens.Micro ((^.))
import System.OsPath.Posix qualified as OP


data PackageMeta = PackageMeta
  { distBase    :: !PackageName
  , distVersion :: !Version
  , pkgBase     :: !Text
  , pkgPath     :: !Text
  , categories  :: ![Text]
    -- ^Basically represents @MASTER_SITES@
  , origin      :: !PackageURI
  , maintainer  :: !Text
  , homepage    :: !Text
  , comment     :: !Text
  , description :: !Text
  , license     :: !Text
  , changeLog   :: !(Maybe TL.Text)
  , flags       :: !FlagMap
  , unrestrict  :: !(Set Text)
  , components  :: ![ComponentMeta]
  }
  deriving (Data, Show)

-- |Construct a 'PackageMeta' from a given package description. This
-- function does not take account of the current state of the PKGPATH, that
-- is, it doesn't read its @Makefile@ even if it exists.
summariseCabal :: RawMeta -> CLI PackageMeta
summariseCabal rawMeta
  = do path     <- (T.pack <$>) . OP.decodeUtf =<< CLI.pkgPath
       base     <- (T.pack <$>) . OP.decodeUtf =<< CLI.pkgBase
       cat      <- (T.pack <$>) . OP.decodeUtf =<< CLI.pkgCategory
       mtr      <- CLI.maintainer
       fs       <- CLI.pkgFlags
       (cs, ts) <- extractComponents gpd
       pure PackageMeta
         { distBase    = C.pkgName . PD.package $ pd
         , distVersion = C.pkgVersion . PD.package $ pd
         , pkgBase     = base
         , pkgPath     = path
         , categories  = [cat]
         , origin      = rmPackageURI rawMeta
         , maintainer  = fromMaybe "pkgsrc-users@NetBSD.org" mtr
         , homepage    = T.pack . ST.fromShortText . PD.homepage $ pd
         , comment     = T.pack . ST.fromShortText . PD.synopsis $ pd
         , description = extractDescription pd
         , license     = extractLicense pd
         , changeLog   = rmChangeLog rawMeta
         , flags       = fs
         , unrestrict  = ts
         , components  = cs
         }
  where
    gpd :: GenericPackageDescription
    gpd = rmGPD rawMeta

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

hasLibraries :: PackageMeta -> Bool
hasLibraries
  = any ((== Library) . (^. cType)) . components

hasForeignLibs :: PackageMeta -> Bool
hasForeignLibs
  = any ((== ForeignLib) . (^. cType)) . components

hasExecutables :: PackageMeta -> Bool
hasExecutables
  = any ((== Executable) . (^. cType)) . components
