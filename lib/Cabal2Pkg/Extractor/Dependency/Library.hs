{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Cabal2Pkg.Extractor.Dependency.Library
  ( LibDep(..)
  , extractLibDep
  ) where

import Cabal2Pkg.CmdLine (CLI, installedPkgs, srcDb)
import Control.Monad (join)
import Data.Data (Data)
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.PackageIndex qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.InstalledPackageInfo qualified as C
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import System.OsPath.Posix (pstr)
import System.OsPath.Posix qualified as OP


-- |Dependency on a library provided by either the compiler or a pkgsrc
-- package.
data LibDep
  = KnownBundledLib
    { -- |The name of Cabal package, such as @"base"@. This constructor is
      -- used when 'Cabal2Pkg.Extractor.summariseCabal' finds that the
      -- dependency is bundled with the compiler.
      name :: !PackageName
      -- |The version of the library it found.
    , version :: !Version
      -- |The range of versions that is acceptable.
    , verRange :: !VersionRange
    }
  | KnownPkgsrcLib
    { -- |The name of Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- finds that the dependency is packaged in pkgsrc.
      name :: !PackageName
      -- |The PKGPATH, such as @"math/hs-semigroupoids"@.
    , pkgPath :: !Text
      -- |The PKGBASE, such as @"hs-semigroupoids"@.
    , pkgBase :: !Text
      -- |The version of the library it found.
    , version :: !Version
      -- |The range of versions that is acceptable.
    , verRange :: !VersionRange
    }
  | UnknownLib
    { -- |The name of a Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- cannot find the corresponding package in pkgsrc or bundled
      -- libraries in the compiler.
      name :: !PackageName
      -- |The range of versions that is acceptable.
    , verRange :: !VersionRange
    }
  deriving (Data, Show)

-- |Equality ignores anything but 'name' so that dependencies are
-- deduplicated.
instance Eq LibDep where
  a == b =
    case a of
      KnownBundledLib {} ->
        case b of
          KnownBundledLib {} -> name a == name b
          KnownPkgsrcLib  {} -> False
          UnknownLib      {} -> False
      KnownPkgsrcLib {} ->
        case b of
          KnownBundledLib {} -> False
          KnownPkgsrcLib  {} -> name a == name b
          UnknownLib      {} -> False
      UnknownLib {} ->
        case b of
          KnownBundledLib {} -> False
          KnownPkgsrcLib  {} -> False
          UnknownLib      {} -> name a == name b

-- |Ordering ignores anything but 'name' so that dependencies are
-- deduplicated.
instance Ord LibDep where
  compare a b =
    case a of
      KnownBundledLib {} ->
        case b of
          KnownBundledLib {} -> compare (name a) (name b)
          KnownPkgsrcLib  {} -> LT
          UnknownLib      {} -> LT
      KnownPkgsrcLib {} ->
        case b of
          KnownBundledLib {} -> GT
          KnownPkgsrcLib  {} -> compare (name a) (name b)
          UnknownLib      {} -> LT
      UnknownLib {} ->
        case b of
          KnownBundledLib {} -> GT
          KnownPkgsrcLib  {} -> GT
          UnknownLib      {} -> compare (name a) (name b)


extractLibDep :: C.Dependency -> CLI LibDep
extractLibDep dep
  = do ipi <- installedPkgs
       case lookupBundled ipi (C.depPkgName dep) of
         Just ver -> pure $ bundled ver
         Nothing  ->
           do m <- findPkgsrcPkg (C.depPkgName dep)
              case m of
                Just (path, base, ver) -> pure $ found path base ver
                Nothing                -> pure notFound
  where
    bundled :: Version -> LibDep
    bundled ver =
      KnownBundledLib
      { name     = C.depPkgName dep
      , version  = ver
      , verRange = C.depVerRange dep
      }

    found :: Text -> Text -> Version -> LibDep
    found path base ver =
      KnownPkgsrcLib
      { name     = C.depPkgName dep
      , pkgPath  = path
      , pkgBase  = base
      , version  = ver
      , verRange = C.depVerRange dep
      }

    notFound :: LibDep
    notFound =
      UnknownLib
      { name     = C.depPkgName dep
      , verRange = C.depVerRange dep
      }

lookupBundled :: C.InstalledPackageIndex -> C.PackageName -> Maybe Version
lookupBundled ipi name
  = case C.lookupPackageName ipi name of
      (ver, pkg : _) : _ ->
        -- NOTE: The package database does not have explicit fields
        -- indicating whether the package is bundled with the compiler. For
        -- now we consider packages whose "hs-libraries" field ends with
        -- "-inplace" to be bundled ones, but this is a fragile test.
        case C.hsLibraries pkg of
          lib : _
            | "-inplace" `L.isSuffixOf` lib ->
                Just ver
          _ ->
            Nothing
      _ ->
        Nothing

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Only packages that include @mk/haskell.mk@ are
-- returned. The function returns a tuple of its @PKGPATH@, @PKGBASE@, and
-- @PKGVERSION_NOREV@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe (Text, Text, Version))
findPkgsrcPkg name
  = do db <- srcDb
       -- Would it be beneficial to perform these two searches
       -- concurrently? I'd say no, because most of the time packages with
       -- the prefix "hs-" is what we would find, and the other search
       -- would just be a waste of CPU cycles.
       name' <- OP.encodeUtf . C.unPackageName $ name
       p0    <- SrcDb.findPackageByNameCI db ([pstr|hs-|] <> name')
       case p0 of
         Just p  -> found p
         Nothing ->
           do p1 <- SrcDb.findPackageByNameCI db name'
              join <$> traverse found p1
  where
    found :: Package CLI -> CLI (Maybe (Text, Text, Version))
    found pkg =
      do hask <- SrcDb.includesHaskellMk pkg
         if hask
           then do ver  <- toCabalVer =<< SrcDb.pkgVersionNoRev pkg
                   path <- T.pack <$> OP.decodeUtf (SrcDb.pkgPath pkg)
                   base <- SrcDb.pkgBase pkg
                   pure $ Just (path, base, ver)
           else pure Nothing

    toCabalVer :: MonadFail m => Text -> m Version
    toCabalVer = either fail pure . eitherParsec . T.unpack
