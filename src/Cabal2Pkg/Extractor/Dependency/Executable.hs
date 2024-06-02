{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor.Dependency.Executable
  ( ExeDep(..)
  , extractExeDep
  ) where

import Cabal2Pkg.CmdLine (CLI, srcDb)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Types.ExeDependency qualified as C
import Distribution.Types.PackageName qualified as C


-- |Dependency on a tool provided by a pkgsrc package.
data ExeDep
  = KnownExe
    { -- |The name of the tool to be listed in @USE_TOOLS@.
      name :: !Text
    }
  | UnknownExe
    { -- |The name of a Cabal package, such as @"alex"@. This constructor
      -- is used when 'Cabal2Pkg.Extractor.summariseCabal' cannot find the
      -- corresponding package in pkgsrc.
      name :: !Text
    }
  deriving (Eq, Show)


-- |Return 'Nothing' if the dependency is bundled with the compiler.
extractExeDep :: C.ExeDependency -> CLI (Maybe ExeDep)
extractExeDep (C.ExeDependency pkgName _ _)
  | isBuiltin pkgName = pure Nothing
  | otherwise =
      do m <- findPkgsrcPkg pkgName
         case m of
           Just name ->
             pure . Just . KnownExe $ name
           Nothing ->
             pure . Just . UnknownExe . T.pack . C.unPackageName $ pkgName

isBuiltin :: C.PackageName -> Bool
isBuiltin = flip elem builtins
  where
    -- I hate hard-coding these but tools aren't registered to the package
    -- database so there's no other options.
    builtins :: [C.PackageName]
    builtins = [ "haddock"
               , "hsc2hs"
               ]

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Whether it includes @mk/haskell.mk@ or not is
-- irrelevant because we don't care in what language it's implemented. The
-- function returns the name of the tool to be listed in @USE_TOOLS@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe Text)
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
    name' :: Text
    name' = T.pack . C.unPackageName $ name
