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
import Data.CaseInsensitive as CI
import Data.Char (toTitle)
import Data.Data (Data)
import Data.List.Extra (drop1)
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
import Lens.Micro.Platform ((^.), (^?), (%~), _head, to)
import System.OsPath.Posix qualified as OP


data PackageMeta = PackageMeta
  { distBase    :: !PackageName
  , distVersion :: !Version
  , pkgBase     :: !Text
  , pkgPath     :: !Text
  , categories  :: ![Text]
    -- ^Basically represents @MASTER_SITES@
  , origin      :: !PackageURI
  , maintainer  :: !(Maybe Text)
  , owner       :: !(Maybe Text)
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
       owr      <- CLI.owner
       fs       <- CLI.pkgFlags
       (cs, ts) <- extractComponents gpd
       pure PackageMeta
         { distBase    = C.pkgName . PD.package $ pd
         , distVersion = C.pkgVersion . PD.package $ pd
         , pkgBase     = base
         , pkgPath     = path
         , categories  = [cat | cat /= "wip"]
         , origin      = rmPackageURI rawMeta
         , maintainer  = mtr
         , owner       = owr
         , homepage    = T.pack . ST.fromShortText . PD.homepage $ pd
         , comment     = extractComment pd
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

extractComment :: PackageDescription -> Text
extractComment =
  dropPeriod
  . capitaliseHead
  . dropArticle
  . T.pack . ST.fromShortText . PD.synopsis
  where
    dropArticle :: Text -> Text
    dropArticle comm =
      case comm ^? to T.words . _head . to CI.mk of
        Just w | w == "A" || w == "An" ->
          -- This is an English indefinite article. Drop it and
          -- capitalise the next word.
          T.unwords . drop1 . T.words $ comm
        _ ->
          comm

    capitaliseHead :: Text -> Text
    capitaliseHead = _head %~ toTitle

    dropPeriod :: Text -> Text
    dropPeriod comm =
      case T.unsnoc comm of
        Just (comm', '.') -> comm'
        _                 -> comm

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
