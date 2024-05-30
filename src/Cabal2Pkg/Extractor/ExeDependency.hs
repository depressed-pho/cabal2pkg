{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor.ExeDependency
  ( ExeDependency(..)
  , extractExeDependency
  ) where

import Cabal2Pkg.CmdLine (CLI, srcDb)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Types.ExeDependency qualified as C
import Distribution.Types.PackageName qualified as C
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS


-- |Dependency on a tool provided by a pkgsrc package.
data ExeDependency
  = KnownExeDependency
    { -- |The name of the tool to be listed in @USE_TOOLS@.
      name :: !ShortText
    }
  | UnknownExeDependency
    { -- |The name of a Cabal package, such as @"alex"@. This constructor
      -- is used when 'Cabal2Pkg.Extractor.summariseCabal' cannot find the
      -- corresponding package in pkgsrc.
      name :: !ShortText
    }
  deriving Show


extractExeDependency :: C.ExeDependency -> CLI ExeDependency
extractExeDependency (C.ExeDependency pkgName _ _)
  = do m <- findPkgsrcPkg pkgName
       case m of
         Just name ->
           pure . KnownExeDependency $ name
         Nothing ->
           pure . UnknownExeDependency . TS.fromString . C.unPackageName $ pkgName


-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Whether it includes @mk/haskell.mk@ or not is
-- irrelevant because we don't care in what language it's implemented. The
-- function returns the name of the tool to be listed in @USE_TOOLS@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe ShortText)
findPkgsrcPkg name
  = do db <- srcDb
       -- Would it be beneficial to perform these two searches
       -- concurrently? I'd say no, because most of the time packages
       -- without the prefix "hs-" is what we would find, and the other
       -- search would just be a waste of CPU cycles.
       p0 <- SrcDb.findPackageCI db name'
       case p0 of
         Just _  -> pure $ Just name'
         Nothing ->
           do p1 <- SrcDb.findPackageCI db ("hs-" <> name')
              pure $ const name' <$> p1
  where
    name' :: ShortText
    name' = TS.fromString . C.unPackageName $ name
