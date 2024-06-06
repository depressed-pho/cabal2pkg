{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange qualified as C
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T


-- |Dependency on a library provided by either the compiler or a pkgsrc
-- package.
data LibDep
  = KnownLib
    { -- |The name of Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- finds that the dependency is packaged in pkgsrc.
      name :: !Text
      -- |The PKGPATH, such as @"math/hs-semigroupoids"@.
    , pkgPath :: !Text
    }
  | UnknownLib
    { -- |The name of a Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- cannot find the corresponding package in pkgsrc or bundled
      -- libraries in GHC.
      name :: !Text
    }
  deriving (Data, Eq, Show)


-- |Return @(md, mt)@ where @md@ is 'Nothing' if the dependency is bundled
-- with the compiler, and @mt@ is the name of Cabal package if it needs to
-- be listed in @HASKELL_UNRESTRICT_DEPENDENCIES@.
extractLibDep :: C.Dependency -> CLI (Maybe LibDep, Maybe Text)
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
    name' :: Text
    name' = T.pack . C.unPackageName . C.depPkgName $ dep

    needsU :: Version -> Maybe Text
    needsU ver
      | not $ C.withinRange ver (C.depVerRange dep)
          = Just name'
      | otherwise
          = Nothing

    bundled :: Version -> (Maybe LibDep, Maybe Text)
    bundled ver
      = (Nothing, needsU ver)

    found :: Text -> Version -> (Maybe LibDep, Maybe Text)
    found path ver
      = (Just (KnownLib name' path), needsU ver)

    notFound :: (Maybe LibDep, Maybe Text)
    notFound
      = (Just (UnknownLib name'), Nothing)

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
