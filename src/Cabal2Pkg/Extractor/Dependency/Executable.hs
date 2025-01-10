{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Cabal2Pkg.Extractor.Dependency.Executable
  ( ExeDep(..)
  , extractExeDep
  ) where

import Cabal2Pkg.CmdLine (CLI, progDb, srcDb)
import Data.Data (Data)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Program.Db qualified as C
import Distribution.Simple.Program.Types qualified as C
import Distribution.Types.ExeDependency qualified as C
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import System.OsPath.Posix (pstr)
import System.OsPath.Posix qualified as OP


-- |Dependency on a tool provided by a pkgsrc package.
data ExeDep
  = KnownBundledExe
    { -- |The name of tool, such as @"hsc2hs"@. This constructor is used
      -- when 'Cabal2Pkg.Extractor.summariseCabal' finds that the
      -- dependency is bundled with the compiler.
      name :: !PackageName
      -- |The version of the tool it found.
    , version :: !Version
      -- |The range of versions that is acceptable.
    , verRange :: !VersionRange
    }
  | KnownPkgsrcExe
    { -- |The name of a tool to be listed in @USE_TOOLS@. This constructor
      -- is used when 'Cabal2Pkg.Extractor.summariseCabal' finds that the
      -- dependency is packaged in pkgsrc.
      name :: !PackageName
      -- |The PKGPATH, such as @"devel/happy"@.
    , pkgPath :: !Text
      -- |The version of the tool it found.
    , version :: !Version
      -- |The range of versions that is acceptable.
    , verRange :: !VersionRange
    }
  | UnknownExe
    { -- |The name of Cabal package, such as @"alex"@. This constructor is
      -- used when 'Cabal2Pkg.Extractor.summariseCabal' cannot find the
      -- corresponding package in pkgsrc.
      name :: !PackageName
    }
  deriving (Data, Eq, Show)


extractExeDep :: C.ExeDependency -> CLI ExeDep
extractExeDep (C.ExeDependency pkgName _ range)
  = do progs <- progDb
       case lookupBundled progs pkgName of
         Just ver -> pure $ bundled ver
         Nothing  ->
           do m <- findPkgsrcPkg pkgName
              case m of
                Just (path, ver) -> pure $ found path ver
                Nothing          -> pure notFound
  where
    bundled :: Version -> ExeDep
    bundled ver =
      KnownBundledExe
      { name     = pkgName
      , version  = ver
      , verRange = range
      }

    found :: Text -> Version -> ExeDep
    found path ver =
      KnownPkgsrcExe
      { name     = pkgName
      , pkgPath  = path
      , version  = ver
      , verRange = range
      }

    notFound :: ExeDep
    notFound = UnknownExe { name = pkgName }

lookupBundled :: C.ProgramDb -> C.PackageName -> Maybe Version
lookupBundled progs pkgName
  = do prog  <- C.lookupKnownProgram (C.unPackageName pkgName) progs
       prog' <- C.lookupProgram prog progs
       C.programVersion prog'

-- |Search for a pkgsrc package case-insensitively, both with and without
-- the @hs-@ prefix. Whether it includes @mk/haskell.mk@ or not is
-- irrelevant because we don't care in what language it's implemented. The
-- function returns a pair of its @PKGPATH@ and @PKGVERSION_NOREV@.
findPkgsrcPkg :: C.PackageName -> CLI (Maybe (Text, Version))
findPkgsrcPkg name
  = do db <- srcDb
       -- Would it be beneficial to perform these two searches
       -- concurrently? I'd say no, because most of the time packages
       -- without the prefix "hs-" is what we would find, and the other
       -- search would just be a waste of CPU cycles.
       name' <- OP.encodeUtf . C.unPackageName $ name
       p0    <- SrcDb.findPackageByNameCI db name'
       case p0 of
         Just p  -> Just <$> found p
         Nothing ->
           do p1 <- SrcDb.findPackageByNameCI db ([pstr|hs-|] <> name')
              traverse found p1
  where
    found :: Package CLI -> CLI (Text, Version)
    found pkg =
      do ver  <- toCabalVer =<< SrcDb.pkgVersionNoRev pkg
         path <- T.pack <$> OP.decodeUtf (SrcDb.pkgPath pkg)
         pure (path, ver)

    toCabalVer :: MonadFail m => Text -> m Version
    toCabalVer = either fail pure . eitherParsec . T.unpack
