{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), FlagMap, debug, fatal, info, warn, pkgPath, srcDb
  , distDir, canonPkgDir, origPkgDir, makeCmd, runMake, withPkgFlagsHidden
  , withPkgFlagsModified, withMaintainer, withOwner, wantCommitMsg )
import Cabal2Pkg.Command.Common
  ( command, option, fetchMeta, shouldHaveBuildlink3 )
import Cabal2Pkg.Extractor (PackageMeta(distBase, distVersion, origin))
import Cabal2Pkg.Generator.Buildlink3 (genBuildlink3)
import Cabal2Pkg.Generator.CommitMsg (genUpdateMsg)
import Cabal2Pkg.Generator.Description (genDESCR)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Cabal2Pkg.Pretty (Emphasised(..), Quoted(..), prettyAnsi)
import Cabal2Pkg.Site
  ( PackageURI(..), isFromLocalFS, isFromHackage, parsePackageURI
  , reconstructPackageURI )
import Cabal2Pkg.Site.Hackage (HackageDist(..))
import Cabal2Pkg.Site.Hackage qualified as Hackage
import Control.Exception.Safe (MonadThrow, assert, catch, throw)
import Control.Monad (unless, when)
import Control.Monad.Extra (maybeM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.Merge (hasMarkers, merge)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec, explicitEitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Flag qualified as C
import Distribution.Types.PackageId (PackageIdentifier(pkgName, pkgVersion))
import Distribution.Types.Version (Version)
import GHC.Stack (HasCallStack)
import Prelude hiding (readFile, writeFile)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Directory.PosixPath (removePathForcibly, renameFile)
import System.File.PosixPath.Alt (readFile, writeFile, writeFreshFile)
import System.IO.Error (isDoesNotExistError)
import System.OsPath.Posix ((</>), PosixPath, PosixString, pstr)
import System.OsPath.Posix qualified as OP

run :: HasCallStack => UpdateOptions -> CLI ()
run opts@(UpdateOptions {..}) =
  do -- Before doing anything expensive, see if files have conflict
     -- markers. Abort if there are any.
     mapM_ checkConflicts [ [pstr|DESCR|]
                          , [pstr|Makefile|]
                          , [pstr|buildlink3.mk|]
                          ]

     -- Look the package up in the source database. We need to know things
     -- like its current version.
     path <- pkgPath
     info $ PP.hsep [ "Examining the current state of"
                    , prettyAnsi path <> "..."
                    ]

     db   <- srcDb
     pkg  <- do mpkg <- SrcDb.findPackageByPath db path
                case mpkg of
                  Just pkg -> pure pkg
                  Nothing  ->
                    fatal $ PP.hsep [ prettyAnsi path
                                    , "doesn't look like a pkgsrc package. It has no"
                                    , prettyAnsi [pstr|Makefile|] <> PP.pretty '.'
                                    ]

     -- If the user requested a version from Hackage, we can take a fast
     -- route getting the set of available versions and see if there's
     -- actually an update.
     pkgId  <- packageId pkg
     pkgURI <- maybe (pure . Hackage $ HackageDist (pkgName pkgId) Nothing)
                     (parsePackageURI (Just $ pkgName pkgId))
                     optPackageURI
     let cont = examineNewMeta opts pkg pkgId
     case pkgURI of
       HTTP   {} -> cont pkgURI
       File   {} -> cont pkgURI
       GitHub {} -> cont pkgURI
       GitLab {} -> cont pkgURI
       Hackage (HackageDist name mVer) ->
         do assert (name == pkgName pkgId) (pure ())
            av <- Hackage.fetchAvailableVersions name
            case mVer of
              Nothing ->
                -- The user requested the latest version. Is the newest
                -- non-deprecated version any newer than the current one?
                let latest = Hackage.latestPreferred av
                in
                  if latest > pkgVersion pkgId
                  then cont (Hackage $ HackageDist name (Just latest)) -- Yes it is.
                  else alreadyLatest path pkgId
              Just ver ->
                -- The user requested a specific version. Does it exist?
                -- Is it any newer then the current one? Is it preferred?
                case M.lookup ver (Hackage.unAV av) of
                  Just Hackage.Normal ->
                    case ver `compare` pkgVersion pkgId of
                      GT             -> cont pkgURI -- Yes it is.
                      EQ             -> alreadyExact path ver
                      -- It's not but the user explicitly asked to do it.
                      LT | optForce  -> downgradeForced path ver pkgId >> cont pkgURI
                         | otherwise -> downgradeRefused path ver pkgId
                  Just Hackage.Deprecated ->
                    case ver `compare` pkgVersion pkgId of
                      GT | optForce  -> deprUpdateForced path ver >> cont pkgURI
                      GT | otherwise -> deprUpdateRefused path ver
                      EQ             -> alreadyExactDepr path ver
                      LT | optForce  -> deprDowngradeForced path ver pkgId >> cont pkgURI
                         | otherwise -> deprDowngradeRefused path ver pkgId
                  Nothing ->
                    unavailable path ver

checkConflicts :: HasCallStack => PosixPath -> CLI ()
checkConflicts name =
  ( do file <- readFile' name
       when (hasMarkers file) . fatal $
         PP.hsep [ prettyAnsi name
                 , "appears to have conflict markers."
                 , "Please resolve conflicts before updating the package."
                 ]
  ) `catch` \(e :: IOError) ->
              unless (isDoesNotExistError e) $
                throw e

alreadyLatest :: HasCallStack => PosixPath -> PackageIdentifier -> CLI ()
alreadyLatest path pkgId =
  info $ PP.hsep [ prettyAnsi path
                 , "is at version"
                 , prettyAnsi (pkgVersion pkgId)
                 , "but it's already the latest one."
                 ]

alreadyExact :: HasCallStack => PosixPath -> Version -> CLI ()
alreadyExact path ver =
  info $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's already at that exact version."
                 ]

downgradeForced :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
downgradeForced path ver pkgId =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's older than the current version"
                 , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                 , "Proceeding to downgrade it anyway because you explicitly asked to."
                 ]

downgradeRefused :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
downgradeRefused path ver pkgId =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but it's older than the current version"
                  , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                  , "If you really want to downgrade it, re-run"
                  , command "update"
                  , "with"
                  , option "-f"
                  ]

deprUpdateForced :: HasCallStack => PosixPath -> Version -> CLI ()
deprUpdateForced path ver =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's been marked as deprecated on Hackage,"
                 , "which usually means it has known defects."
                 , "Proceeding to use this version anyway because you explicitly asked to."
                 ]

deprUpdateRefused :: HasCallStack => PosixPath -> Version -> CLI ()
deprUpdateRefused path ver =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but it's been marked as deprecated on Hackage,"
                  , "which usually means it has known defects."
                  , "If you really want to use this version, re-run"
                  , command "update"
                  , "with"
                  , option "-f"
                  ]

alreadyExactDepr :: HasCallStack => PosixPath -> Version -> CLI ()
alreadyExactDepr path ver =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's already at that exact version."
                 , "Note that it's been marked as deprecated on Hackage,"
                 , "which usually means it has known defects."
                 ]

deprDowngradeForced :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
deprDowngradeForced path ver pkgId =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's older than the current version"
                 , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                 , "The requested version is also marked as deprecated on Hackage,"
                 , "which usually means it has known defects."
                 , "Proceeding to downgrade it to this version anyway because you explicitly asked to."
                 ]

deprDowngradeRefused :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
deprDowngradeRefused path ver pkgId =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but it's older than the current version"
                  , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                  , "The requested version is also marked as deprecated on Hackage,"
                  , "which usually means it has known defects."
                  , "If you really want to downgrade it to this version, re-run"
                  , command "update"
                  , "with"
                  , option "-f"
                  ]

unavailable :: HasCallStack => PosixPath -> Version -> CLI ()
unavailable path ver =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but this specific version is not available in Hackage."
                  ]

noUpstreamForced :: HasCallStack => PosixPath -> CLI ()
noUpstreamForced path =
  warn $ PP.hsep [ "You are updating"
                 , prettyAnsi path
                 , "with a tarball on the local file system."
                 , command "update"
                 , "is assuming that the package has lost its upstream."
                 , "Proceeding to set"
                 , prettyAnsi (Quoted "MASTER_SITES")
                 , "to empty because you explicitly asked to."
                 ]

noUpstreamRefused :: HasCallStack => PosixPath -> CLI ()
noUpstreamRefused path =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "with a tarball on the local file system."
                  , command "update"
                  , "is assuming that the package has lost its upstream, i.e. its"
                  , prettyAnsi (Quoted "MASTER_SITES")
                  , "needs to be set to empty. But since the package"
                  , prettyAnsi (Emphasised "did")
                  , "have an upstream before, this is highly likely to be a mistake."
                  , "If you really want to do this, re-run"
                  , command "update"
                  , "with"
                  , option "-f"
                  ]

-- This is a continuation of 'run'. Obtain the package metadata of the
-- requested version (or the latest one). If it isn't newer than
-- PKGVERSION_NOREV, then it's clear we can bail out now. We can skip this
-- check if the package is from Hackage because we have already done it in
-- that case.
examineNewMeta :: HasCallStack
               => UpdateOptions
               -> SrcDb.Package CLI
               -> PackageIdentifier
               -> PackageURI
               -> CLI ()
examineNewMeta opts@(UpdateOptions {..}) pkg pkgId pkgURI =
  do path <- pkgPath
     when (isFromLocalFS pkgURI) $
       -- The updated package has no upstreams.
       do hadMs <- not . null <$> SrcDb.masterSites pkg
          when hadMs $
            if optForce
            then noUpstreamForced path
            else noUpstreamRefused path
     mtr  <- SrcDb.maintainer pkg
     owr  <- SrcDb.owner      pkg
     -- This is new metadata. Package flags on the command line should be
     -- merged into old ones found in Makefile. The former should have a
     -- higher precedence over the latter.
     oldFlags <- packageFlags pkg
     newMeta  <- withPkgFlagsModified (<> oldFlags)
                 . withMaintainer mtr
                 . withOwner owr
                 $ fetchMeta pkgURI
     let cont = examineOldMeta opts pkg oldFlags newMeta
     case pkgURI of
       Hackage {} -> cont
       _ ->
         let ver = distVersion newMeta
         in
           case ver `compare` pkgVersion pkgId of
             GT             -> cont -- It's newer.
             EQ             -> alreadyLatest path pkgId
             -- It's older but the user explicitly asked to do it.
             LT | optForce  -> downgradeForced path ver pkgId >> cont
                | otherwise -> downgradeRefused path ver pkgId

-- This is a continuation of 'examineNewMeta'. Obtain the package metadata
-- of the current version somehow. If it's in Hackage we can just query the
-- Hackage API using its DISTNAME, because DISTNAME is identical to the
-- package ID. But if not... we first need to run "make fetch" and try to
-- locate a tarball in "${DISTDIR}/${DIST_SUBDIR}".
--
-- We could just do the latter all the time, which would be fine if we have
-- already downloaded the tarball. Otherwise we would end up downloading
-- one that is soon going to be useless. A single .cabal file is almost
-- always smaller than the entire tarball even if it's uncompressed so...
examineOldMeta :: HasCallStack
               => UpdateOptions
               -> SrcDb.Package CLI
               -> FlagMap
               -> PackageMeta
               -> CLI ()
examineOldMeta opts pkg oldFlags newMeta =
  do path      <- pkgPath
     oldPkgURI <- let err = fatal $ PP.hsep [ prettyAnsi path
                                            , "has no MASTER_SITES."
                                            , command mempty
                                            , "cannot figure out how to update this package."
                                            ]
                  in
                    maybeM err pure $ reconstructPackageURI pkg
     mtr       <- SrcDb.maintainer pkg
     owr       <- SrcDb.owner      pkg
     -- As this is old metadata, no package flags on the command line
     -- arguments should be taken account of.
     oldMeta   <- withPkgFlagsHidden
                  . withPkgFlagsModified (const oldFlags)
                  . withMaintainer mtr
                  . withOwner owr
                  $ if isFromHackage oldPkgURI then
                      fetchMeta oldPkgURI
                    else
                      fetchDistFile oldPkgURI
     -- Renaming packages is currently not supported. It will probably
     -- never be.
     when (distBase oldMeta /= distBase newMeta) . fatal $
       PP.hsep [ "Package names don't match. You requested to update"
               , prettyAnsi (distBase oldMeta)
               , "with a different package"
               , prettyAnsi (distBase newMeta) <> PP.dot
               , "If you really need to rename a package, then sorry about that."
               , command mempty
               , "cannot do that."
               ]
     applyChanges opts pkg oldMeta newMeta
  where
    fetchDistFile :: PackageURI -> CLI PackageMeta
    fetchDistFile pkgURI =
      do runMake ["fetch"]
         dir    <- distDir
         subDir <- SrcDb.distSubDir pkg
         name   <- SrcDb.distName pkg
         sufx   <- SrcDb.extractSufx pkg
         path   <- OP.decodeUtf
                   $ maybe dir (dir </>) subDir </> name <> sufx
         meta   <- fetchMeta (File path)
         -- Correct the 'origin' because it might not be accurate.
         pure $ meta { origin = pkgURI }

-- |This is a continuation of 'examineOldMeta'. Perform a 3-way merge based
-- on the current set of files and the old and new package metadata.
applyChanges :: HasCallStack
             => UpdateOptions
             -> SrcDb.Package CLI
             -> PackageMeta
             -> PackageMeta
             -> CLI ()
applyChanges (UpdateOptions {..}) pkg oldMeta newMeta =
  do labelCur <- do distName <- (T.pack <$>) . OP.decodeUtf =<< SrcDb.distName pkg
                    pure $ distName <> " (current)"
     let labelBase = mconcat [ T.pack . prettyShow . distBase    $ oldMeta
                             , "-"
                             , T.pack . prettyShow . distVersion $ oldMeta
                             , " (merge base)"
                             ]
         labelNew  = mconcat [ T.pack . prettyShow . distBase    $ newMeta
                             , "-"
                             , T.pack . prettyShow . distVersion $ newMeta
                             , " (new)"
                             ]
     let update :: PosixPath -> (PackageMeta -> TL.Text) -> (TL.Text -> TL.Text) -> CLI ()
         update name gen preprocess =
           do let base = gen oldMeta
                  new  = gen newMeta
              cur <- preprocess <$> readFile' name
              let merged = merge optMarkerStyle
                                 (labelCur , cur )
                                 (labelBase, base)
                                 (labelNew , new )
              if merged == cur
                then info $ prettyAnsi name <+> "needs no changes"
                else do name' <- OP.decodeUtf name
                        debug $ mconcat [ "Merged" <+> PP.pretty name'
                                        , ":\n"
                                        , PP.pretty (TL.strip merged)
                                        ]
                        -- Rename the existing file before writing the
                        -- merged one. We will delete old files but we do
                        -- it at the very end.
                        renameFile' name (name <> backupSuffix)
                        writeFreshFile' name merged
                        -- Warn if it has conflicts.
                        when (hasMarkers merged) . warn $
                          PP.hsep [ prettyAnsi name
                                  , "has merge conflicts."
                                  , "Please don't forget to resolve them."
                                  ]

     -- These files are generated from the package description, and they
     -- should always exist.
     update [pstr|DESCR|]    genDESCR    id
     update [pstr|Makefile|] genMakefile stripMakefileRev

     -- buildlink3.mk is a tricky one.
     let bl3 = [pstr|buildlink3.mk|]
     case shouldHaveBuildlink3 newMeta of
       Just False ->
         -- The updated package shouldn't have buildlink3.mk. If the
         -- package currently has one, we should even delete it.
         deleteFile' True bl3

       Just True ->
         -- The updated package should have buildlink3.mk. If the package
         -- currently doesn't have one, create it as if this were the
         -- "init" command.
         update bl3 genBuildlink3 stripBl3Rev
         `catch` \(e :: IOError) ->
                   if isDoesNotExistError e then
                     writeFreshFile' bl3 (genBuildlink3 newMeta)
                   else
                     throw e

       Nothing ->
         -- The updated package may have buildlink3.mk but it doesn't have
         -- to. If the package currently has one, update it like any other
         -- files. Otherwise leave it non-existent.
         update bl3 genBuildlink3 stripBl3Rev
         `catch` \(e :: IOError) ->
                   unless (isDoesNotExistError e) $
                     throw e

     wantCMsg <- wantCommitMsg
     when wantCMsg $
       do let cMsg = genUpdateMsg oldMeta newMeta
          debug $ "Generated COMMIT_MSG:\n" <> PP.pretty (TL.strip cMsg)
          writeFile' [pstr|COMMIT_MSG|] cMsg

     -- PLIST cannot be directly generated from the package description. We
     -- have no choice but to leave it unchanged.

     -- Now that we updated all the files, we can delete backup now.
     mapM_ deleteBackup [ [pstr|DESCR|]
                        , [pstr|Makefile|]
                        , [pstr|buildlink3.mk|]
                        ]

     -- distinfo needs to be updated too, but the only way to do it is to
     -- run make(1). This means we can only do it if there are no conflicts
     -- in Makefile.
     mk <- readFile' [pstr|Makefile|]
     if hasMarkers mk
       then do make <- T.pack <$> (OP.decodeUtf . OP.takeFileName =<< makeCmd)
               warn $ PP.hsep [ command mempty
                              , "cannot update"
                              , prettyAnsi [pstr|distinfo|]
                              , "because there are unresolved conflicts in"
                              , prettyAnsi [pstr|Makefile|] <> PP.dot
                              , "Be sure to run"
                              , prettyAnsi (Quoted (PP.pretty make <+> "distinfo"))
                              , "after resolving them."
                              ]
       else do info "Updating distinfo..."
               runMake ["distinfo"]

backupSuffix :: PosixString
backupSuffix = [pstr|.cabal2pkg.sav|]

deleteBackup :: PosixPath -> CLI ()
deleteBackup name =
  deleteFile' False (name <> backupSuffix)

-- |Strip PKGREVISION from a Makefile, because it should be reset whenever
-- a package is updated.
stripMakefileRev :: TL.Text -> TL.Text
stripMakefileRev = TL.unlines . filter p . TL.lines
  where
    p :: TL.Text -> Bool
    p line
      | "PKGREVISION=" `TL.isPrefixOf` line = False
      | otherwise                           = True

-- |Strip PKGREVISION from a buildlink3.mk, because it should be reset
-- whenever a package is updated.
stripBl3Rev :: TL.Text -> TL.Text
stripBl3Rev = TL.unlines . (go <$>) . TL.lines
  where
    go :: TL.Text -> TL.Text
    go line
      | "BUILDLINK_ABI_DEPENDS." `TL.isPrefixOf` line =
          -- Split the line with "nb" as a delimiter. If the last piece
          -- consists only of digits, then it's clearly the revision we
          -- want to remove.
          case L.unsnoc (TL.splitOn "nb" line) of
            Just (xs, x)
              | TL.all isDigit x -> TL.intercalate "nb" xs
            _ -> line
      | otherwise = line

readFile' :: PosixPath -> CLI TL.Text
readFile' name =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     lbs <- readFile cfp
     case TL.decodeUtf8' lbs of
       Right txt -> pure txt
       Left  e   -> fatal $ PP.hsep [ "Cannot read"
                                    , prettyAnsi ofp <> PP.pretty ':'
                                    , PP.viaShow e
                                    ]

renameFile' :: PosixPath -> PosixPath -> CLI ()
renameFile' from to =
  do cfpFrom <- (</> from) <$> canonPkgDir
     cfpTo   <- (</> to  ) <$> canonPkgDir
     renameFile cfpFrom cfpTo

writeFile' :: PosixPath -> TL.Text -> CLI ()
writeFile' name txt =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     writeFile cfp (TL.encodeUtf8 txt)
     info $ "Wrote " <> prettyAnsi ofp

writeFreshFile' :: PosixPath -> TL.Text -> CLI ()
writeFreshFile' name txt =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     writeFreshFile cfp (TL.encodeUtf8 txt)
     info $ "Wrote " <> prettyAnsi ofp

deleteFile' :: Bool -> PosixPath -> CLI ()
deleteFile' showMsg name =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     removePathForcibly cfp
     when showMsg . info $ "Deleted " <> prettyAnsi ofp

-- |Reinterpret DISTNAME as a Cabal package identifier. This is guaranteed
-- to be a valid interpretation as long as the package Makefile includes
-- @mk/haskell.mk@.
packageId :: (MonadThrow m, MonadUnliftIO m)
          => SrcDb.Package m
          -> m PackageIdentifier
packageId pkg =
  do isHask <- SrcDb.includesHaskellMk pkg
     unless isHask . fatal $
       PP.hsep [ prettyAnsi (SrcDb.pkgPath pkg)
               , "doesn't look like a Haskell package. It doesn't include"
               , prettyAnsi [pstr|mk/haskell.mk|] <> PP.pretty '.'
               ]
     distName <- SrcDb.distName pkg
     dnStr    <- OP.decodeUtf distName
     case eitherParsec dnStr of
       Right pkgId -> pure pkgId
       Left  e     -> fatal (PP.viaShow e)

-- |Extract Cabal package flags from CONFIGURE_ARGS.
packageFlags :: (MonadThrow m, MonadUnliftIO m)
             => SrcDb.Package m
             -> m FlagMap
packageFlags pkg =
  do args <- SrcDb.configureArgs pkg
     go args
  where
    go :: MonadThrow m => [Text] -> m FlagMap
    go []     = pure mempty
    go (a:as)
      | a == "-f" =
          -- The next argument is a flag assignment.
          case as of
            []     -> err $ PP.hsep [ "Found an option"
                                    , option "-f"
                                    , "but it has no arguments"
                                    ]
            (b:bs) -> do fm <- parseFM b
                         (fm <>) <$> go bs

      | "-f" `T.isPrefixOf` a =
          -- The remaining part of the argument is a flag assignment.
          do fm <- parseFM (T.drop (T.length "-f") a)
             (fm <>) <$> go as

      | "--flags=" `T.isPrefixOf` a =
          -- The remaining part of the argument is a flag assignment.
          do fm <- parseFM (T.drop (T.length "--flags=") a)
             (fm <>) <$> go as

      | otherwise =
          go as

    parseFM :: MonadThrow m => Text -> m FlagMap
    parseFM txt =
      case explicitEitherParsec C.legacyParsecFlagAssignment $ T.unpack txt of
        Left   e -> err $ PP.hsep [ "Found an invalid flag assignment"
                                  , prettyAnsi (Quoted $ PP.pretty txt) <> PP.pretty ':'
                                  , PP.pretty e
                                  ]
        Right fa -> pure . M.fromList . C.unFlagAssignment $ fa

    err :: MonadThrow m => Doc AnsiStyle -> m a
    err e = fatal $ PP.hsep [ prettyAnsi (SrcDb.pkgPath pkg)
                            , e
                            ]
