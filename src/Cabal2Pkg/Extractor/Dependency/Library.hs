{-# LANGUAGE DeriveAnyClass #-} -- for deriving Hashable
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency.Library
  ( LibDep(..)
  , extractLibDep
  ) where

import Cabal2Pkg.CmdLine (CLI, installedPkgs, srcDb)
import Control.Monad (join)
import Data.Hashable (Hashable(..))
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.PackageIndex qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.InstalledPackageInfo qualified as C
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange qualified as C
import Data.List (isSuffixOf)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import GHC.Generics (Generic)


-- |Dependency on a library provided by a pkgsrc package.
data LibDep
  = KnownLib
    { -- |The PKGPATH, such as @"math/hs-semigroupoids"@.
      pkgPath :: !ShortText
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | UnknownLib
    { -- |The name of a Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- cannot find the corresponding package in pkgsrc or bundled
      -- libraries in GHC.
      name :: !ShortText
    }
  deriving (Eq, Generic, Hashable, Show)


-- |Return 'Nothing' if the dependency is bundled with the compiler.
extractLibDep :: C.Dependency -> CLI (Maybe LibDep)
extractLibDep dep
  = do ipi <- installedPkgs
       if isBuiltin ipi (C.depPkgName dep)
         then pure Nothing
         else do m <- findPkgsrcPkg (C.depPkgName dep)
                 case m of
                   Just (path, ver) ->
                     pure . Just $ found path ver
                   Nothing ->
                     pure . Just $ notFound
  where
    found :: ShortText -> Version -> LibDep
    found path ver
      = KnownLib
        { pkgPath            = path
        , needsUnrestricting = not $ C.withinRange ver (C.depVerRange dep)
        }

    notFound :: LibDep
    notFound
      = UnknownLib
        { name = TS.fromString . C.unPackageName . C.depPkgName $ dep
        }

isBuiltin :: C.InstalledPackageIndex -> C.PackageName -> Bool
isBuiltin ipi name
  = case C.lookupPackageName ipi name of
      ((_, (pkg:_)):_) ->
        -- NOTE: The package database does not have explicit fields
        -- indicating whether the package is bundled with the compiler. For
        -- now we consider packages whose "hs-libraries" field ends with
        -- "-inplace" to be bundled ones, but this is a fragile test.
        case C.hsLibraries pkg of
          (lib:_) -> "-inplace" `isSuffixOf` lib
          _       -> False
      _ ->
        False

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Only packages that include @mk/haskell.mk@ are
-- returned. The function returns a pair of its @PKGPATH@ and
-- @PKGVERSION_NOREV@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe (ShortText, Version))
findPkgsrcPkg name
  = do db <- srcDb
       -- Would it be beneficial to perform these two searches
       -- concurrently? I'd say no, because most of the time packages with
       -- the prefix "hs-" is what we would find, and the other search
       -- would just be a waste of CPU cycles.
       p0 <- SrcDb.findPackageCI db ("hs-" <> name')
       case p0 of
         Just p  -> found p
         Nothing ->
           do p1 <- SrcDb.findPackageCI db name'
              join <$> traverse found p1
  where
    name' :: ShortText
    name' = TS.fromString . C.unPackageName $ name

    found :: Package CLI -> CLI (Maybe (ShortText, Version))
    found pkg
      = do hask <- SrcDb.includesHaskellMk pkg
           if hask
             then do ver <- toCabalVer =<< SrcDb.pkgVersionNoRev pkg
                     pure $ Just (SrcDb.pkgPath pkg, ver)
             else pure Nothing

    toCabalVer :: MonadFail m => ShortText -> m Version
    toCabalVer = either fail pure . eitherParsec . TS.toString
