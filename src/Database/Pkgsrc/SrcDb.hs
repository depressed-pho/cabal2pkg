{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Pkgsrc.SrcDb
  ( -- * Types
    SrcDb
  , Category
  , Package

    -- * Construction
  , createSrcDb

    -- * Querying
  , distDir
  , masterSiteHaskellHackage
  , findPackageByPath
  , findPackageByName
  , findPackageByNameCI

    -- * Accessors
  , pkgPath
  , distName
  , distSubDir
  , pkgName
  , pkgVersionNoRev
  , pkgRevision
  , extractSufx
  , maintainer
  , masterSites
  , githubProject
  , gitlabProject
  , configureArgs
  , includesHaskellMk
  ) where

import Control.Applicative (Alternative(..), asum, optional)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad ((<=<), forM_)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Either.Combinators (mapLeft)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO.Utf8 qualified as U8
import Data.Text.Read qualified as TR
import GHC.Stack (HasCallStack)
import Network.URI (URI, parseAbsoluteURI)
import System.Directory.PosixPath
  ( doesDirectoryExist, doesFileExist, listDirectory )
import System.IO (hClose)
import System.OsPath.Posix ((</>), PosixPath, PosixString, pstr)
import System.OsPath.Posix qualified as OP
import System.OsString.Posix.Instances ()
import System.Process.Typed qualified as PT
import UnliftIO.Async (mapConcurrently)


data SrcDb m
  = SrcDb
    { dCategories                  :: !(HashMap PosixPath (Deferred m (Category m)))
    , dDISTDIR                     :: !(Deferred m PosixPath)
    , dMASTER_SITE_HASKELL_HACKAGE :: !(Deferred m [URI])
    }

-- |A category of pkgsrc packages. It has nothing to do with category
-- theory.
data Category m
  = Category
    { cPackages   :: !(HashMap     PosixPath  (Deferred m (Package m)))
    , cPackagesCI :: !(HashMap (CI PosixPath) (Deferred m (Package m)))
    }

data Package m
  = Package
    { pPKGPATH           :: PosixPath
    , pDISTNAME          :: Deferred m PosixString
    , pDIST_SUBDIR       :: Deferred m (Maybe PosixPath)
    , pPKGNAME           :: Deferred m Text
    , pPKGVERSION_NOREV  :: Deferred m Text
    , pPKGREVISION       :: Deferred m (Maybe Int)
    , pEXTRACT_SUFX      :: Deferred m PosixString
    , pMAINTAINER        :: Deferred m Text
    , pMASTER_SITES      :: Deferred m [URI]
    , pGITHUB_PROJECT    :: Deferred m (Maybe Text)
    , pGITLAB_PROJECT    :: Deferred m (Maybe Text)
    , pCONFIGURE_ARGS    :: Deferred m [Text]
    , pIncludesHaskellMk :: Deferred m Bool
    }

type VarMap = HashMap Text Text

newtype Getter a = Getter { runGetter :: Text -> Either String a }

instance Functor Getter where
  fmap :: (a -> b) -> Getter a -> Getter b
  fmap f (Getter g) = Getter $ (f <$>) . g

instance Applicative Getter where
  pure :: a -> Getter a
  pure = Getter . const . Right

  liftA2 :: (a -> b -> c) -> Getter a -> Getter b -> Getter c
  liftA2 f g1 g2 =
    Getter $ \txt ->
    do a <- runGetter g1 txt
       b <- runGetter g2 txt
       pure (f a b)

instance Alternative Getter where
  empty :: Getter a
  empty = Getter . const . Left $ "empty"

  (<|>) :: Getter a -> Getter a -> Getter a
  g1 <|> g2 =
    Getter $ \txt ->
    case runGetter g1 txt of
      Left _ -> runGetter g2 txt
      r      -> r

  some :: Getter a -> Getter [a]
  some g =
    Getter $ \txt ->
    case runGetter (many g) txt of
      Right [] -> Left "variable empty or undefined"
      r        -> r

  many :: forall a. Getter a -> Getter [a]
  many g = Getter $ go . mkWords
    where
      go :: [Text] -> Either String [a]
      go []     = Right []
      go (w:ws) = case runGetter g w of
                    Left  e -> Left e
                    Right a -> (a :) <$> go ws

-- |This is similar to T.words but we need to concatenate words joined
-- with '\ '.
mkWords :: Text -> [Text]
mkWords = go False mempty
  where
    -- This is obviously inefficient but optimisation is probably not worth
    -- it.
    go :: Bool -> Text -> Text -> [Text]
    go esc word txt =
      case T.uncons txt of
        Nothing ->
          [word | not (T.null word)]

        Just (c, rest)
          | c == ' '  -> if esc then
                           go False (word `T.snoc` c) rest
                         else
                           let ws = go False mempty rest
                           in
                             if T.null word then
                               ws
                             else
                               word : ws
          | c == '\\' -> go (not esc) (word `T.snoc` c) rest
          | otherwise -> go False     (word `T.snoc` c) rest

get :: HasCallStack => Text -> Getter a -> VarMap -> a
get var g vm =
  case HM.lookup var vm of
    Nothing  -> error $ T.unpack var <> ": variable not found"
    Just txt -> case runGetter g txt of
                  Left  e -> error $ T.unpack var <> ": " <> e
                  Right a -> a

exists :: Getter Bool
exists = Getter $ Right . not . T.null

text :: Getter Text
text =
  Getter $ \txt ->
  if T.null txt then
    Left "variable empty or undefined"
  else
    Right txt

-- |This doesn't accept an empty string.
posixStr :: Getter PosixString
posixStr =
  Getter $ \txt ->
  if T.null txt then
    Left "variable empty or undefined"
  else
    mapLeft show . OP.encodeUtf . T.unpack $ txt

absURI :: Getter URI
absURI =
  Getter $ \txt ->
  case parseAbsoluteURI . T.unpack $ txt of
    Nothing -> Left $ "not an absolute URI: " <> T.unpack txt
    Just u  -> Right u

int :: Getter Int
int =
  Getter $ \txt ->
  case TR.decimal txt of
    Right (n, r)
      | T.null r  -> Right n
      | otherwise -> Left $ "not a decimal number: " <> T.unpack txt
    Left e ->
      Left e

-- |Create a database of pkgsrc packages.
createSrcDb :: forall m.
               (MonadThrow m, MonadUnliftIO m)
            => PosixPath -- ^The path to BSD make(1) command.
            -> PosixPath -- ^The root directory of pkgsrc tree, typically @/usr/pkgsrc@.
            -> m (SrcDb m)
createSrcDb makePath root =
  do let dirPath = -- Any package will do.
           root </> [pstr|pkgtools|] </> [pstr|pkg_install|]
     vars <- defer $ getMakeVars makePath dirPath
                       [ "DISTDIR"
                       , "MASTER_SITE_HASKELL_HACKAGE"
                       ]
     cs   <- cats
     pure $ SrcDb { dCategories = cs
                  , dDISTDIR    = get "DISTDIR" posixStr <$> vars
                  , dMASTER_SITE_HASKELL_HACKAGE =
                      get "MASTER_SITE_HASKELL_HACKAGE" (many absURI) <$> vars
                  }
  where
    cats :: m (HashMap PosixPath (Deferred m (Category m)))
    cats = listDirectory root
           >>= filterCats root
           >>= (HM.fromList <$>) . mapM deferCat

    deferCat :: PosixPath -> m (PosixPath, Deferred m (Category m))
    deferCat catName =
      (catName, ) <$> defer (mkCat catName)

    mkCat :: PosixPath -> m (Category m)
    mkCat catName =
      do pkgs <- scanPkgs makePath root catName
         pure Category
           { cPackages   = pkgs
           , cPackagesCI = HM.mapKeys CI.mk pkgs
           }

-- |Only include directories that has a Makefile including
-- @../mk/misc/category.mk@. Also exclude @wip@.
filterCats :: (MonadThrow m, MonadUnliftIO m) => PosixPath -> [PosixPath] -> m [PosixPath]
filterCats root = (catMaybes <$>) . mapConcurrently go
  where
    go :: (MonadIO m, MonadThrow m) => PosixPath -> m (Maybe PosixPath)
    go ent
      | ent == [pstr|wip|] = pure Nothing
      | otherwise          =
          do isDir <- doesDirectoryExist (root </> ent)
             if isDir
               then do let mk = root </> ent </> [pstr|Makefile|]
                       hasMk <- doesFileExist mk
                       if hasMk
                         then do i <- includesCatMk mk
                                 if i
                                   then pure (Just ent)
                                   else pure Nothing
                         else pure Nothing
               else pure Nothing

    includesCatMk :: (MonadIO m, MonadThrow m) => PosixPath -> m Bool
    includesCatMk file
      = do fp  <- OP.decodeUtf file
           txt <- liftIO $ U8.readFile fp
           case T.breakOnAll "\"../mk/misc/category.mk\"" txt of
             [] -> pure False
             _  -> pure True

scanPkgs :: forall m.
            (MonadThrow m, MonadUnliftIO m)
         => PosixPath
         -> PosixPath
         -> PosixPath
         -> m (HashMap PosixPath (Deferred m (Package m)))
scanPkgs makePath root catName =
  listDirectory catPath
  >>= filterPkgs catPath
  >>= (HM.fromList <$>) . mapM deferPkg
  where
    catPath :: PosixPath
    catPath = root </> catName

    deferPkg :: PosixPath -> m (PosixPath, Deferred m (Package m))
    deferPkg dirName =
      (dirName, ) <$> defer (mkPkg dirName)

    mkPkg :: PosixPath -> m (Package m)
    mkPkg dirName =
      do let dirPath = catPath </> dirName
         vars <- defer $ getMakeVars makePath dirPath
                           [ "DISTNAME"
                           , "DIST_SUBDIR"
                           , "PKGNAME"
                           , "PKGVERSION_NOREV"
                           , "PKGREVISION"
                           , "EXTRACT_SUFX"
                           , "MAINTAINER"
                           , "MASTER_SITES"
                           , "GITHUB_PROJECT"
                           , "GITLAB_PROJECT"
                           , "CONFIGURE_ARGS"
                           , "HASKELL_PKG_NAME"
                           ]
         pure Package
           { pPKGPATH           = catName </> dirName
           , pDISTNAME          = get "DISTNAME"         posixStr            <$> vars
           , pDIST_SUBDIR       = get "DIST_SUBDIR"      (optional posixStr) <$> vars
           , pPKGNAME           = get "PKGNAME"          text                <$> vars
           , pPKGVERSION_NOREV  = get "PKGVERSION_NOREV" text                <$> vars
           , pPKGREVISION       = get "PKGREVISION"      (optional int)      <$> vars
           , pEXTRACT_SUFX      = get "EXTRACT_SUFX"     posixStr            <$> vars
           , pMAINTAINER        = get "MAINTAINER"       text                <$> vars
           , pMASTER_SITES      = get "MASTER_SITES"     (many absURI)       <$> vars
           , pGITHUB_PROJECT    = get "GITHUB_PROJECT"   (optional text)     <$> vars
           , pGITLAB_PROJECT    = get "GITLAB_PROJECT"   (optional text)     <$> vars
           , pCONFIGURE_ARGS    = get "CONFIGURE_ARGS"   (many text)         <$> vars
           , pIncludesHaskellMk = get "HASKELL_PKG_NAME" exists              <$> vars
           }

-- |Extract a set of variables from a Makefile in an absolute path to a
-- package directory. This is obviously the slowest part of
-- cabal2pkg. Parallelise calls of it at all costs.
getMakeVars :: (MonadThrow m, MonadUnliftIO m)
            => PosixPath
            -> PosixPath
            -> HashSet Text
            -> m VarMap
getMakeVars makePath dirPath vars
  = do make' <- OP.decodeUtf makePath
       dir'  <- OP.decodeUtf dirPath
       let conf = PT.setStdin PT.createPipe
                  . PT.setStdout PT.byteStringOutput
                  . PT.setWorkingDir dir'
                  $ PT.proc make' ["-f", "-", "-f", "Makefile", "x"]
       PT.withProcessWait_ conf $ \p ->
         liftIO $
         do let stdin = PT.getStdin p
                vars' = HS.toList vars
            U8.hPutStrLn stdin ".PHONY: x"
            U8.hPutStrLn stdin "x:"
            forM_ vars' $ \var ->
              do U8.hPutStr stdin "\t@printf '%s\\0' \"${"
                 U8.hPutStr stdin var
                 U8.hPutStrLn stdin "}\""
            hClose stdin

            out <- either throw pure . T.decodeUtf8' . BL.toStrict
                   =<< atomically (PT.getStdout p)
            let vals = T.split (== '\0') out
            pure . HM.fromList $ zip vars' vals

-- |Only include directories that has a Makefile.
filterPkgs :: MonadUnliftIO m => PosixPath -> [PosixPath] -> m [PosixPath]
filterPkgs catPath = (catMaybes <$>) . mapConcurrently go
  where
    go :: MonadIO m => PosixPath -> m (Maybe PosixPath)
    go ent
      = do isDir <- doesDirectoryExist (catPath </> ent)
           if isDir
             then do let mk = catPath </> ent </> [pstr|Makefile|]
                     hasMk <- doesFileExist mk
                     if hasMk
                       then pure (Just ent)
                       else pure Nothing
             else pure Nothing

distDir :: MonadUnliftIO m => SrcDb m -> m PosixPath
distDir = force . dDISTDIR

masterSiteHaskellHackage :: MonadUnliftIO m => SrcDb m -> m [URI]
masterSiteHaskellHackage = force . dMASTER_SITE_HASKELL_HACKAGE

-- |Search for a package by a PKGPATH e.g. @"devel/hs-lens"
findPackageByPath :: MonadUnliftIO m => SrcDb m -> PosixPath -> m (Maybe (Package m))
findPackageByPath (SrcDb {..}) path =
  runMaybeT $
  do (cat, name) <- hoistMaybe $ readPkgPath path
     dps         <- hoistMaybe $ HM.lookup cat dCategories
     ps          <- lift $ force dps
     dp          <- hoistMaybe $ HM.lookup name (cPackages ps)
     lift (force dp)

readPkgPath :: PosixPath -> Maybe (PosixPath, PosixPath)
readPkgPath path =
  case OP.splitDirectories path of
    [cat, name] -> Just (cat, name)
    _           -> Nothing

-- |Search for a package that exactly matches with the given name. The
-- search is performed against the name of directories but not against
-- @PKGNAME@'s.
findPackageByName :: MonadUnliftIO m => SrcDb m -> PosixPath -> m (Maybe (Package m))
findPackageByName db name =
  findPackageCommon db (HM.lookup name . cPackages)

-- |A variant of 'findPackage' but performs search case-insensitively.
findPackageByNameCI :: MonadUnliftIO m => SrcDb m -> PosixPath -> m (Maybe (Package m))
findPackageByNameCI db name =
  findPackageCommon db (HM.lookup (CI.mk name) . cPackagesCI)

findPackageCommon :: forall m.
                     MonadUnliftIO m
                  => SrcDb m
                  -> (Category m -> Maybe (Deferred m (Package m)))
                  -> m (Maybe (Package m))
findPackageCommon (SrcDb {..}) q
  = asum <$> mapConcurrently (go <=< force) (HM.elems dCategories)
  where
    go :: Category m -> m (Maybe (Package m))
    go = traverse force . q

pkgPath :: Package m -> PosixPath
pkgPath = pPKGPATH

distName :: MonadUnliftIO m => Package m -> m PosixString
distName = force . pDISTNAME

distSubDir :: MonadUnliftIO m => Package m -> m (Maybe PosixString)
distSubDir = force . pDIST_SUBDIR

pkgName :: MonadUnliftIO m => Package m -> m Text
pkgName = force . pPKGNAME

pkgVersionNoRev :: MonadUnliftIO m => Package m -> m Text
pkgVersionNoRev = force . pPKGVERSION_NOREV

pkgRevision :: MonadUnliftIO m => Package m -> m (Maybe Int)
pkgRevision = force . pPKGREVISION

extractSufx :: MonadUnliftIO m => Package m -> m PosixString
extractSufx = force . pEXTRACT_SUFX

maintainer :: MonadUnliftIO m => Package m -> m Text
maintainer = force . pMAINTAINER

masterSites :: MonadUnliftIO m => Package m -> m [URI]
masterSites = force . pMASTER_SITES

githubProject :: MonadUnliftIO m => Package m -> m (Maybe Text)
githubProject = force . pGITHUB_PROJECT

gitlabProject :: MonadUnliftIO m => Package m -> m (Maybe Text)
gitlabProject = force . pGITLAB_PROJECT

configureArgs :: MonadUnliftIO m => Package m -> m [Text]
configureArgs = force . pCONFIGURE_ARGS

includesHaskellMk :: MonadUnliftIO m => Package m -> m Bool
includesHaskellMk = force . pIncludesHaskellMk
