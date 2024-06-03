{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Extractor.Dependency.Executable
  ( ExeDep(..)
  , extractExeDep
  ) where

import Cabal2Pkg.CmdLine (CLI, progDb, srcDb)
import Control.Monad (join)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Program.Db qualified as C
import Distribution.Simple.Program.Types qualified as C
import Distribution.Types.ExeDependency qualified as C
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange qualified as C


-- |Dependency on a tool provided by a pkgsrc package.
data ExeDep
  = BundledExe
    { -- |The name of a tool, such as @"hsc2hs"@. This constructor is used
      -- when 'Cabal2Pkg.Extractor.summariseCabal' finds that the tool is
      -- bundled with the compiler.
      name :: !Text
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | KnownExe
    { -- |The name of a tool to be listed in @USE_TOOLS@.
      name :: !Text
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | UnknownExe
    { -- |The name of a Cabal package, such as @"alex"@. This constructor
      -- is used when 'Cabal2Pkg.Extractor.summariseCabal' cannot find the
      -- corresponding package in pkgsrc.
      name :: !Text
    }
  deriving (Eq, Show)


extractExeDep :: C.ExeDependency -> CLI ExeDep
extractExeDep (C.ExeDependency pkgName _ verRange)
  = do progs <- progDb
       case lookupBundled progs pkgName of
         Just ver -> pure $ bundled ver
         Nothing  ->
           do m <- findPkgsrcPkg pkgName
              case m of
                Just (name, ver) -> pure $ found name ver
                Nothing          -> pure $ notFound
  where
    bundled :: Version -> ExeDep
    bundled ver
      = BundledExe
        { name               = T.pack $ C.unPackageName pkgName
        , needsUnrestricting = not $ C.withinRange ver verRange
        }

    found :: Text -> Version -> ExeDep
    found name ver
      = KnownExe
        { name               = name
        , needsUnrestricting = not $ C.withinRange ver verRange
        }

    notFound :: ExeDep
    notFound
      = UnknownExe
        { name = T.pack $ C.unPackageName pkgName
        }

lookupBundled :: C.ProgramDb -> C.PackageName -> Maybe Version
lookupBundled progs pkgName
  = do prog  <- C.lookupKnownProgram (C.unPackageName pkgName) progs
       prog' <- C.lookupProgram prog progs
       ver   <- C.programVersion prog'
       pure ver

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Whether it includes @mk/haskell.mk@ or not is
-- irrelevant because we don't care in what language it's implemented. The
-- function returns a pair of the name of the tool to be listed in
-- @USE_TOOLS@ and its @PKGVERSION_NOREV@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe (Text, Version))
findPkgsrcPkg name
  = do db <- srcDb
       -- Would it be beneficial to perform these two searches
       -- concurrently? I'd say no, because most of the time packages
       -- without the prefix "hs-" is what we would find, and the other
       -- search would just be a waste of CPU cycles.
       p0 <- SrcDb.findPackageCI db name'
       case p0 of
         Just p  -> found p
         Nothing ->
           do p1 <- SrcDb.findPackageCI db ("hs-" <> name')
              join <$> traverse found p1
  where
    name' :: Text
    name' = T.pack . C.unPackageName $ name

    found :: Package CLI -> CLI (Maybe (Text, Version))
    found pkg
      = do ver <- toCabalVer =<< SrcDb.pkgVersionNoRev pkg
           pure $ Just (name', ver)

    toCabalVer :: MonadFail m => Text -> m Version
    toCabalVer = either fail pure . eitherParsec . T.unpack
