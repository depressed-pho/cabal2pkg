{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), FlagMap, debug, fatal, fillColumn, info, warn
  , srcDb, distDir, canonPkgDir, origPkgDir, pkgBase, pkgPath, makeCmd, runMake
  , withPkgFlagsHidden, withPkgFlagsModified, withMaintainer, withOwner
  , wantCommitMsg )
import Cabal2Pkg.Command.Common
  ( command, command', option, fetchMeta, shouldHaveBuildlink3, warnDeps )
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
import Data.Coerce (coerce)
import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
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
import Language.BMake.AST
  (ExactPrint, Makefile, exactPrintMakefile, parseMakefile)
import Language.BMake.AST qualified as AST
import Prelude hiding (mod, readFile, writeFile)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.Directory.PosixPath (doesFileExist, removePathForcibly, renameFile)
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
                  Nothing  -> fatal $
                              PP.hsep [ prettyAnsi path
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
                  else alreadyLatest path pkgId >> maybeRegenFiles opts pkg
              Just ver ->
                -- The user requested a specific version. Does it exist?
                -- Is it any newer then the current one? Is it preferred?
                case M.lookup ver (Hackage.unAV av) of
                  Just Hackage.Normal ->
                    case ver `compare` pkgVersion pkgId of
                      GT             -> cont pkgURI -- Yes it is.
                      EQ             -> alreadyExact path ver >> maybeRegenFiles opts pkg
                      -- It's not but the user explicitly asked to do it.
                      LT | optForce  -> downgradeForced path ver pkgId >> cont pkgURI
                         | otherwise -> downgradeRefused path ver pkgId
                  Just Hackage.Unpreferred ->
                    case ver `compare` pkgVersion pkgId of
                      GT             -> unprUpdateRequested path ver >> cont pkgURI
                      EQ             -> alreadyExactUnpr path ver >> maybeRegenFiles opts pkg
                      LT | optForce  -> unprDowngradeForced path ver pkgId >> cont pkgURI
                         | otherwise -> unprDowngradeRefused path ver pkgId
                  Just Hackage.Deprecated ->
                    case ver `compare` pkgVersion pkgId of
                      GT | optForce  -> deprUpdateForced path ver >> cont pkgURI
                      GT | otherwise -> deprUpdateRefused path ver
                      EQ             -> alreadyExactDepr path ver >> maybeRegenFiles opts pkg
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
                 , "Proceeding to downgrade it anyway because you explicitly asked for."
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

unprUpdateRequested :: HasCallStack => PosixPath -> Version -> CLI ()
unprUpdateRequested path ver =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's been marked as unpreferred on Hackage."
                 , "Proceeding to use this version anyway because you explicitly asked for."
                 ]

alreadyExactUnpr :: HasCallStack => PosixPath -> Version -> CLI ()
alreadyExactUnpr path ver =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's already at that exact version."
                 , "Note that it's been marked as unpreferred on Hackage."
                 ]

unprDowngradeForced :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
unprDowngradeForced path ver pkgId =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's older than the current version"
                 , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                 , "The requested version is also marked as unpreferred on Hackage."
                 , "Proceeding to downgrade it to this version anyway because"
                 , "you explicitly asked for."
                 ]

unprDowngradeRefused :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
unprDowngradeRefused path ver pkgId =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but it's older than the current version"
                  , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                  , "The requested version is also marked as unpreferred on Hackage."
                  , "If you really want to downgrade it to this version, re-run"
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
                 , "Proceeding to use this version anyway because you explicitly asked for."
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
                 , "Proceeding to downgrade it to this version anyway because"
                 , "you explicitly asked for."
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
                 , "to empty because you explicitly asked for."
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
     warnDeps newMeta
     let cont = examineOldMeta opts pkg oldFlags newMeta
     case pkgURI of
       Hackage {} -> cont
       _ ->
         let ver = distVersion newMeta
         in
           case ver `compare` pkgVersion pkgId of
             GT             -> cont -- It's newer.
             EQ             -> alreadyLatest path pkgId >> maybeRegenFiles opts pkg
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
  do oldPkgURI <- uriFromPkg pkg
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
                      fetchDistFile pkg
     -- Renaming packages is currently not supported. It will probably
     -- never be.
     when (distBase oldMeta /= distBase newMeta) . fatal $
       PP.hsep [ "Package names don't match. You requested to update"
               , prettyAnsi (distBase oldMeta)
               , "with a different package"
               , prettyAnsi (distBase newMeta) <> PP.dot
               , "If you really need to rename a package, then sorry about that."
               , command'
               , "cannot do that."
               ]
     applyChanges opts pkg oldMeta newMeta

uriFromPkg :: SrcDb.Package CLI -> CLI PackageURI
uriFromPkg pkg =
  do path <- pkgPath
     let err = fatal $ PP.hsep [ prettyAnsi path
                               , "has no MASTER_SITES."
                               , command'
                               , "cannot figure out how to update this package."
                               ]
     maybeM err pure $ reconstructPackageURI pkg

fetchDistFile :: SrcDb.Package CLI -> CLI PackageMeta
fetchDistFile pkg =
  do runMake ["fetch"]
     dir    <- distDir
     subDir <- SrcDb.distSubDir pkg
     name   <- SrcDb.distName pkg
     sufx   <- SrcDb.extractSufx pkg
     path   <- OP.decodeUtf
               $ maybe dir (dir </>) subDir </> name <> sufx
     meta   <- fetchMeta (File path)
     -- Correct the 'origin' because it's inaccurate due to the use of
     -- (File path) above.
     pkgURI <- uriFromPkg pkg
     pure $ meta { origin = pkgURI }

-- This is a continuation of examineOldMeta. Perform a 3-way merge based
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
     let update :: PosixPath
                -> (PackageMeta -> TL.Text)
                -> (PosixPath -> TL.Text -> CLI TL.Text)
                -> CLI ()
         update name gen preprocess =
           do let new = gen newMeta
              -- We are parsing and preprocessing Makefile we just
              -- generated from oldMeta, which wastes CPU cycles but
              -- there's no good way to avoid it.
              base <- preprocess name (gen oldMeta)
              cur  <- readFile' name
              cur' <- preprocess name cur
              let merged = merge optMarkerStyle
                                 (labelCur , cur')
                                 (labelBase, base)
                                 (labelNew , new )
              if merged == cur
                then info $ prettyAnsi name <+> "needs no changes."
                else do name' <- OP.decodeUtf name
                        debug $ mconcat [ "Merged" <+> PP.pretty name'
                                        , ":\n"
                                        , PP.pretty (TL.strip merged)
                                        ]
                        -- Rename the existing file before writing the
                        -- merged one. We will delete old files but we do
                        -- it at the very end.
                        renameFile' name (name <> backupSuffix)
                        updateFile' name merged
                        -- Warn if it has conflicts.
                        when (hasMarkers merged) . warn $
                          PP.hsep [ prettyAnsi name
                                  , "has merge conflicts."
                                  , "Please don't forget to resolve them."
                                  ]

     -- These files are generated from the package description, and they
     -- should always exist.
     width <- fillColumn
     update [pstr|DESCR|]    (genDESCR width) (const (pure . id))
     update [pstr|Makefile|] genMakefile (ppMakefile (stripUnrestrict . stripMakefileRev))

     -- buildlink3.mk is a tricky one.
     pBase <- (T.pack <$>) . OP.decodeUtf =<< pkgBase
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
         update bl3 genBuildlink3 (ppMakefile (stripBl3Rev pBase))
         `catch` \(e :: IOError) ->
                   if isDoesNotExistError e then
                     writeFile' bl3 (genBuildlink3 newMeta)
                   else
                     throw e

       Nothing ->
         -- The updated package may have buildlink3.mk but it doesn't have
         -- to. If the package currently has one, update it like any other
         -- files. Otherwise leave it non-existent.
         update bl3 genBuildlink3 (ppMakefile (stripBl3Rev pBase))
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
               warn $ PP.hsep [ command'
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
               info $ PP.annotate (PP.colorDull PP.Yellow <> PP.bold)
                      "Successfully updated the package."

-- This is a continuation of applyChanges, which is used only when DISTFILE
-- is the same. Metadata can still have differences because the desired set
-- of package flags can differ.
maybeRegenFiles :: HasCallStack
                => UpdateOptions
                -> SrcDb.Package CLI
                -> CLI ()
maybeRegenFiles (UpdateOptions {..}) pkg =
  do info "Checking if files still needs updating..."

     -- Unlike examineOldMeta we always run "make fetch" and try to locate
     -- a tarball, because the distfile is guaranteed to stay the same.
     oldFlags <- packageFlags pkg
     oldMeta  <- fetchMeta' (const oldFlags)
     newMeta  <- fetchMeta' (<>    oldFlags)
     assert ( distBase    oldMeta == distBase    newMeta &&
              distVersion oldMeta == distVersion newMeta
            ) (pure ())

     distName <- (T.pack <$>) . OP.decodeUtf =<< SrcDb.distName pkg
     let labelCur  = distName <> " (current)"
         labelBase = distName <> " (merge base)"
         labelNew  = distName <> " (updated)"

     let update :: PosixPath
                -> (PackageMeta -> TL.Text)
                -> (PosixPath -> TL.Text -> CLI TL.Text)
                -> CLI Bool -- ^True if there are changes.
         update name gen preprocess =
           do let new = gen newMeta
              base <- preprocess name (gen oldMeta)
              cur  <- readFile' name
              cur' <- preprocess name cur
              let merged = merge optMarkerStyle
                           (labelCur , cur')
                           (labelBase, base)
                           (labelNew , new )
              if merged == cur
                then do info $ prettyAnsi name <+> "needs no changes."
                        pure False
                else do name' <- OP.decodeUtf name
                        debug $ mconcat [ "Merged" <+> PP.pretty name'
                                        , ":\n"
                                        , PP.pretty (TL.strip merged)
                                        ]
                        -- Rename the existing file before writing the
                        -- merged one. We will delete old files but we do
                        -- it at the very end.
                        renameFile' name (name <> backupSuffix)
                        updateFile' name merged
                        -- Warn if it has conflicts.
                        when (hasMarkers merged) . warn $
                          PP.hsep [ prettyAnsi name
                                  , "has merge conflicts."
                                  , "Please don't forget to resolve them."
                                  ]
                        pure True

     width    <- fillColumn
     updates  <- sequence
                 [ update [pstr|DESCR|]    (genDESCR width) (const (pure . id))
                 , update [pstr|Makefile|] genMakefile (ppMakefile (stripUnrestrict . stripMakefileRev))

                 , do pBase <- (T.pack <$>) . OP.decodeUtf =<< pkgBase
                      let bl3 = [pstr|buildlink3.mk|]
                      case shouldHaveBuildlink3 newMeta of
                        Just False ->
                          deleteFile' True bl3 *> pure True

                        Just True ->
                          update bl3 genBuildlink3 (ppMakefile (stripBl3Rev pBase))
                          `catch` \(e :: IOError) ->
                                    if isDoesNotExistError e then
                                      writeFile' bl3 (genBuildlink3 newMeta) *> pure True
                                    else
                                      throw e

                        Nothing ->
                          update bl3 genBuildlink3 (ppMakefile (stripBl3Rev pBase))
                          `catch` \(e :: IOError) ->
                                    if isDoesNotExistError e then
                                      pure False
                                    else
                                      throw e
                 ]
     wantCMsg <- wantCommitMsg
     when (wantCMsg && or updates) $
       do path <- OP.decodeUtf =<< pkgPath
          let cMsg = TL.unlines
                     [ mconcat
                       [ TL.pack path
                       , ": accommodate files to the current environment"
                       ]
                     ]
          debug $ "Generated COMMIT_MSG:\n" <> PP.pretty (TL.strip cMsg)
          writeFile' [pstr|COMMIT_MSG|] cMsg

     if or updates
       then info $ PP.annotate (PP.colorDull PP.Yellow <> PP.bold)
                   "Some files have been changed. Please commit them."
       else info $ PP.annotate (PP.colorDull PP.Green)
                   "No files have been changed."

     -- There can be no changes to PLIST or distfiles ever. Now that we
     -- updated all the files, we can delete backup now.
     mapM_ deleteBackup [ [pstr|DESCR|]
                        , [pstr|Makefile|]
                        , [pstr|buildlink3.mk|]
                        ]
    where
      fetchMeta' :: (FlagMap -> FlagMap) -> CLI PackageMeta
      fetchMeta' mod =
        do mtr <- SrcDb.maintainer pkg
           owr <- SrcDb.owner      pkg
           withPkgFlagsHidden
             . withPkgFlagsModified mod
             . withMaintainer mtr
             . withOwner owr
             $ fetchDistFile pkg

ppMakefile :: (Makefile ExactPrint -> Makefile ExactPrint)
           -> PosixPath
           -> TL.Text
           -> CLI TL.Text
ppMakefile ppAST name cur =
  case parseMakefile cur of
    Left msg ->
      fatal $ PP.hsep [ "Failed to parse"
                      , prettyAnsi name <> PP.colon
                      , PP.pretty msg
                      ]
    Right ast ->
      pure . exactPrintMakefile . ppAST $ ast

backupSuffix :: PosixString
backupSuffix = [pstr|.cabal2pkg.sav|]

deleteBackup :: PosixPath -> CLI ()
deleteBackup name =
  deleteFile' False (name <> backupSuffix)

-- |Strip PKGREVISION from a Makefile, because it should be reset whenever
-- a package is updated.
stripMakefileRev :: Makefile ExactPrint -> Makefile ExactPrint
stripMakefileRev = coerce (filter p)
  where
    p :: AST.Block ExactPrint -> Bool
    p bl
      | AST.BAssignment (AST.Assignment {..}) <- bl
      , AST.Value _ var                       <- aVar = var /= "PKGREVISION"
      | otherwise                                     = True

-- |Strip HASKELL_UNRESTRICT_DEPENDENCIES from a Makefile. These should be
-- completely overwritten rather than patched, because its value is not
-- determined solely by the package itself but also by the surrounding
-- environment (i.e. the versions of packages it depends on, including ones
-- that come along with GHC), and the environment might have changed since
-- when the last time the package was updated. Computing two sets of
-- packages that should be listed in HASKELL_UNRESTRICT_DEPENDENCIES for
-- the older and the newer one, and taking a diff between them, therefore
-- doesn't make sense and creates merge conflicts that aren't worth
-- resolving by hand.
stripUnrestrict :: Makefile ExactPrint -> Makefile ExactPrint
stripUnrestrict = everywhere (mkT f)
  where
    f :: Makefile ExactPrint -> Makefile ExactPrint
    f = coerce go

    go :: [AST.Block ExactPrint] -> [AST.Block ExactPrint]
    go (b0:u:b1:xs)
      -- The line we want to remove is enclosed by blank lines. In this
      -- case we don't want to leave two blank lines: a single blank line
      -- is enough.
      | AST.BBlank (AST.Blank _ Nothing) <- b0
      , AST.BAssignment as               <- u
      , isUnrestrict as
      , AST.BBlank (AST.Blank _ Nothing) <- b1 = b0 : go xs

    go (x:xs)
      | AST.BAssignment as <- x
      , isUnrestrict as         = go xs
      | otherwise               = x : go xs

    go [] = []

    isUnrestrict :: AST.Assignment x -> Bool
    isUnrestrict (AST.Assignment {..})
      | AST.Value _ var <- aVar =
          var == "HASKELL_UNRESTRICT_DEPENDENCIES"

-- |Strip PKGREVISION from a buildlink3.mk, because it should be reset
-- whenever a package is updated.
stripBl3Rev :: Text -> Makefile ExactPrint -> Makefile ExactPrint
stripBl3Rev pBase = everywhere (mkT f)
  where
    f :: AST.Block ExactPrint -> AST.Block ExactPrint
    f bl
      | AST.BAssignment as@(AST.Assignment {..}) <- bl
      , AST.Value _ var                          <- aVar
      , var == "BUILDLINK_ABI_DEPENDS." <> pBase =
          AST.BAssignment $ as { AST.aValues = g <$> aValues }
      | otherwise =
          bl

    -- Split the value with "nb" as a delimiter. If the last piece consists
    -- only of digits, then it's clearly the revision we want to remove.
    g :: AST.Value ExactPrint -> AST.Value ExactPrint
    g v@(AST.Value {..}) =
      case T.breakOnEnd "nb" vText of
        (a, b) | T.all isDigit b -> v { AST.vText = T.dropEnd 2 a } -- Remove "nb" as well.
               | otherwise       -> v

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

updateFile' :: PosixPath -> TL.Text -> CLI ()
updateFile' name txt =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     writeFreshFile cfp (TL.encodeUtf8 txt)
     info $ "Updated " <> prettyAnsi ofp

deleteFile' :: Bool -> PosixPath -> CLI ()
deleteFile' showMsg name =
  do cfp <- (</> name) <$> canonPkgDir
     ofp <- (</> name) <$> origPkgDir
     exi <- doesFileExist cfp
     removePathForcibly cfp
     when (showMsg && exi) . info $ "Deleted " <> prettyAnsi ofp

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
