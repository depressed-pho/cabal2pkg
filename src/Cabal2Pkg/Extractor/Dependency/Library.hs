{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency.Library
  ( LibDep(..)
  , extractLibDep
  ) where

import Cabal2Pkg.CmdLine (CLI, installedPkgs, srcDb)
import Control.Monad (join)
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
import Data.Text (Text)
import Data.Text qualified as T


-- |Dependency on a library provided by either the compiler or a pkgsrc
-- package.
data LibDep
  = BundledLib
    { -- |The name of a Cabal package, such as @"base"@. This constructor
      -- is used when 'Cabal2Pkg.Extractor.summariseCabal' finds that
      -- the dependency is bundled with the compiler.
      name :: !Text
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | KnownLib
    { -- |The PKGPATH, such as @"math/hs-semigroupoids"@.
      pkgPath :: !Text
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | UnknownLib
    { -- |The name of a Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- cannot find the corresponding package in pkgsrc or bundled
      -- libraries in GHC.
      name :: !Text
    }
  deriving (Eq, Show)


extractLibDep :: C.Dependency -> CLI LibDep
extractLibDep dep
  = do ipi <- installedPkgs
       case lookupBundled ipi (C.depPkgName dep) of
         Just ver -> pure $ bundled ver
         Nothing  ->
           do m <- findPkgsrcPkg (C.depPkgName dep)
              case m of
                Just (path, ver) -> pure $ found path ver
                Nothing          -> pure $ notFound
  where
    bundled :: Version -> LibDep
    bundled ver
      = BundledLib
        { name               = T.pack . C.unPackageName . C.depPkgName $ dep
        , needsUnrestricting = not $ C.withinRange ver (C.depVerRange dep)
        }

    found :: Text -> Version -> LibDep
    found path ver
      = KnownLib
        { pkgPath            = path
        , needsUnrestricting = not $ C.withinRange ver (C.depVerRange dep)
        }

    notFound :: LibDep
    notFound
      = UnknownLib
        { name = T.pack . C.unPackageName . C.depPkgName $ dep
        }

lookupBundled :: C.InstalledPackageIndex -> C.PackageName -> Maybe Version
lookupBundled ipi name
  = case C.lookupPackageName ipi name of
      ((ver, (pkg:_)):_) ->
        -- NOTE: The package database does not have explicit fields
        -- indicating whether the package is bundled with the compiler. For
        -- now we consider packages whose "hs-libraries" field ends with
        -- "-inplace" to be bundled ones, but this is a fragile test.
        case C.hsLibraries pkg of
          (lib:_)
            | "-inplace" `isSuffixOf` lib ->
                Just ver
          _ ->
            Nothing
      _ ->
        Nothing

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Only packages that include @mk/haskell.mk@ are
-- returned. The function returns a pair of its @PKGPATH@ and
-- @PKGVERSION_NOREV@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe (Text, Version))
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
    name' :: Text
    name' = T.pack . C.unPackageName $ name

    found :: Package CLI -> CLI (Maybe (Text, Version))
    found pkg
      = do hask <- SrcDb.includesHaskellMk pkg
           if hask
             then do ver <- toCabalVer =<< SrcDb.pkgVersionNoRev pkg
                     pure $ Just (SrcDb.pkgPath pkg, ver)
             else pure Nothing

    toCabalVer :: MonadFail m => Text -> m Version
    toCabalVer = either fail pure . eitherParsec . T.unpack
