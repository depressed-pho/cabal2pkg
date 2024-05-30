{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Pkgsrc.SrcDb
  ( SrcDb
  , Category
  , Package
  , createSrcDb
  , findPackage
  , findPackageCI

    -- Accessors
  , pkgPath
  , pkgName
  , pkgVersionNoRev
  , includesHaskellMk
  ) where

import Control.Applicative (asum)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO.Utf8 qualified as U8
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Data.Text.Short.Orphans ()
import System.Directory.OsPath
  ( doesDirectoryExist, doesFileExist, listDirectory )
import System.IO (hClose)
import System.OsPath ((</>), OsPath, OsString)
import System.OsPath qualified as OP
import System.Process.Typed qualified as PT
import UnliftIO.Async (mapConcurrently)


data SrcDb m
  = SrcDb
    { categories :: !(HashMap ShortText (Deferred m (Category m)))
    }

-- |A category of pkgsrc packages. It has nothing to do with category
-- theory.
data Category m
  = Category
    { packages   :: !(HashMap     ShortText  (Deferred m (Package m)))
    , packagesCI :: !(HashMap (CI ShortText) (Deferred m (Package m)))
    }

data Package m
  = Package
    { pPKGPATH           :: ShortText
    , pPKGNAME           :: Deferred m ShortText
    , pPKGVERSION_NOREV  :: Deferred m ShortText
    , pIncludesHaskellMk :: Deferred m Bool
    }


-- |Create a database of pkgsrc packages.
createSrcDb :: (MonadThrow m, MonadUnliftIO m)
            => OsPath -- ^The path to BSD make(1) command.
            -> OsPath -- ^The root directory of pkgsrc tree, typically @/usr/pkgsrc@.
            -> m (SrcDb m)
createSrcDb makePath root = SrcDb <$> go
  where
    go :: (MonadThrow m, MonadUnliftIO m) => m (HashMap ShortText (Deferred m (Category m)))
    go = liftIO (listDirectory root)
         >>= filterCats root
         >>= (HM.fromList <$>) . mapM deferCat

    deferCat :: (MonadThrow m, MonadUnliftIO m) => OsString -> m (ShortText, Deferred m (Category m))
    deferCat catName
      = (,) <$> (TS.fromString <$> OP.decodeUtf catName)
            <*> (defer $ mkCat catName)

    mkCat :: (MonadThrow m, MonadUnliftIO m) => OsString -> m (Category m)
    mkCat catName
      = do pkgs <- scanPkgs makePath root catName
           pure Category
             { packages   = pkgs
             , packagesCI = HM.mapKeys CI.mk pkgs
             }

os :: String -> OsString
os = either (error . show) id . OP.encodeUtf

-- Only include directories that has a Makefile including
-- "../mk/misc/category.mk". Also exclude "wip".
filterCats :: (MonadThrow m, MonadUnliftIO m) => OsPath -> [OsString] -> m [OsString]
filterCats root = (catMaybes <$>) . mapConcurrently go
  where
    go :: (MonadIO m, MonadThrow m) => OsString -> m (Maybe OsString)
    go ent
      | ent == os "wip" = pure Nothing
      | otherwise       =
          do isDir <- liftIO $ doesDirectoryExist (root </> ent)
             if isDir
               then do let mk = root </> ent </> os "Makefile"
                       hasMk <- liftIO $ doesFileExist mk
                       if hasMk
                         then do i <- includesCatMk mk
                                 if i
                                   then pure (Just ent)
                                   else pure Nothing
                         else pure Nothing
               else pure Nothing

    includesCatMk :: (MonadIO m, MonadThrow m) => OsPath -> m Bool
    includesCatMk file
      = do fp   <- OP.decodeUtf file
           text <- liftIO $ U8.readFile fp
           case T.breakOnAll "\"../mk/misc/category.mk\"" text of
             [] -> pure False
             _  -> pure True

scanPkgs :: forall m.
            (MonadThrow m, MonadUnliftIO m)
         => OsPath
         -> OsPath
         -> OsPath
         -> m (HashMap ShortText (Deferred m (Package m)))
scanPkgs makePath root catName
  = liftIO (listDirectory catPath)
    >>= filterPkgs catPath
    >>= (HM.fromList <$>) . mapM deferPkg
  where
    catPath :: OsPath
    catPath = root </> catName

    deferPkg :: OsString -> m (ShortText, Deferred m (Package m))
    deferPkg dirName
      = (,) <$> (TS.fromString <$> OP.decodeUtf dirName)
            <*> (defer $ mkPkg dirName)

    mkPkg :: OsString -> m (Package m)
    mkPkg dirName
      = do let dirPath = catPath </> dirName
           pkgPathTS <- TS.fromString <$> OP.decodeUtf (catName </> dirName)
           vars      <- defer $ getMakeVars dirPath [ "HASKELL_PKG_NAME"
                                                    , "PKGNAME"
                                                    , "PKGVERSION_NOREV"
                                                    ]
           pure Package
             { pPKGPATH           = pkgPathTS
             , pPKGNAME           = (HM.! "PKGNAME"         ) <$> vars
             , pPKGVERSION_NOREV  = (HM.! "PKGVERSION_NOREV") <$> vars
             , pIncludesHaskellMk = HM.member "HASKELL_PKG_NAME" <$> vars
             }

    -- |This is obviously the slowest part of cabal2pkg. Parallelise calls
    -- of it at all costs.
    getMakeVars :: OsPath
                -> HashSet ShortText
                -> m (HashMap ShortText ShortText)
    getMakeVars dirPath vars
      = do make' <- OP.decodeUtf makePath
           dir'  <- OP.decodeUtf dirPath
           let conf = PT.setStdin PT.createPipe
                    $ PT.setStdout PT.byteStringOutput
                    $ PT.setWorkingDir dir'
                    $ PT.proc make' ["-f", "-", "-f", "Makefile", "x"]
           PT.withProcessWait_ conf $ \p ->
             liftIO $
             do let stdin = PT.getStdin p
                    vars' = HS.toList vars
                U8.hPutStrLn stdin ".PHONY: x"
                U8.hPutStrLn stdin "x:"
                flip mapM_ vars' $ \var ->
                  do U8.hPutStr stdin "\t@printf '%s\\0' \"${"
                     U8.hPutStr stdin $ TS.toText var
                     U8.hPutStrLn stdin "}\""
                hClose stdin

                out <- either throw pure . T.decodeUtf8' . BL.toStrict
                       =<< atomically (PT.getStdout p)
                let vals = TS.fromText <$> T.split (== '\0') out
                pure . HM.fromList $ zip vars' vals

-- |Only include directories that has a Makefile.
filterPkgs :: MonadUnliftIO m => OsPath -> [OsString] -> m [OsString]
filterPkgs catPath = (catMaybes <$>) . mapConcurrently go
  where
    go :: MonadIO m => OsString -> m (Maybe OsString)
    go ent
      = do isDir <- liftIO $ doesDirectoryExist (catPath </> ent)
           if isDir
             then do let mk = catPath </> ent </> os "Makefile"
                     hasMk <- liftIO $ doesFileExist mk
                     if hasMk
                       then pure (Just ent)
                       else pure Nothing
             else pure Nothing

-- |Search for a package that exactly matches with the given name. The
-- search is performed against the name of directories but not against
-- @PKGNAME@'s.
findPackage :: MonadUnliftIO m => SrcDb m -> ShortText -> m (Maybe (Package m))
findPackage db name
  = findPackageCommon db (HM.lookup name . packages)

-- |A variant of 'findPackage' but performs search case-insensitively.
findPackageCI :: MonadUnliftIO m => SrcDb m -> ShortText -> m (Maybe (Package m))
findPackageCI db name
  = findPackageCommon db (HM.lookup (CI.mk name) . packagesCI)

findPackageCommon :: forall m.
                     MonadUnliftIO m
                  => SrcDb m
                  -> (Category m -> Maybe (Deferred m (Package m)))
                  -> m (Maybe (Package m))
findPackageCommon (SrcDb {..}) q
  = asum <$> mapConcurrently ((go =<<) . force) (HM.elems categories)
  where
    go :: Category m -> m (Maybe (Package m))
    go = traverse force . q

pkgPath :: Package m -> ShortText
pkgPath = pPKGPATH

pkgName :: MonadUnliftIO m => Package m -> m ShortText
pkgName = force . pPKGNAME

pkgVersionNoRev :: MonadUnliftIO m => Package m -> m ShortText
pkgVersionNoRev = force . pPKGVERSION_NOREV

includesHaskellMk :: MonadUnliftIO m => Package m -> m Bool
includesHaskellMk = force . pIncludesHaskellMk
