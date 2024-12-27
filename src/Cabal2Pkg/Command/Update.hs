{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), FlagMap, fatal, info, warn, pkgPath, srcDb
  , withPkgFlagsHidden, withPkgFlagsModified, runMake )
import Cabal2Pkg.Command.Common (command, option, fetchMeta)
import Cabal2Pkg.Extractor (PackageMeta(distVersion))
import Cabal2Pkg.Hackage qualified as Hackage
import Cabal2Pkg.PackageURI (PackageURI(..), isFromHackage, parsePackageURI)
import Cabal2Pkg.Pretty (Quoted(..), prettyAnsi)
import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow, assert)
import Control.Monad (unless)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec, explicitEitherParsec)
import Distribution.Types.Flag qualified as C
import Distribution.Types.PackageId (PackageIdentifier(pkgName, pkgVersion))
import Distribution.Types.Version (Version)
import GHC.Stack (HasCallStack)
import Lens.Micro ((&), (%~))
import Network.URI.Lens (uriPathLens)
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.OsPath.Posix ((</>), PosixPath, pstr)
import System.OsPath.Posix qualified as OP

run :: HasCallStack => UpdateOptions -> CLI ()
run opts@(UpdateOptions {..}) =
  do -- Before doing anything expensive, see if files have conflict
     -- markers. Abort if there are any.
     -- FIXME: do it

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
     pkgURI <- maybe (pure $ Hackage (pkgName pkgId) Nothing)
                     (parsePackageURI (Just $ pkgName pkgId))
                     optPackageURI
     let cont = examineNewMeta opts pkg pkgId
     case pkgURI of
       HTTP _ -> cont pkgURI
       File _ -> cont pkgURI
       Hackage name mVer ->
         do assert (name == pkgName pkgId) (pure ())
            av <- Hackage.fetchAvailableVersions name
            case mVer of
              Nothing ->
                -- The user requested the latest version. Is the newest
                -- non-deprecated version any newer than the current one?
                let latest = Hackage.latestPreferred av
                in
                  if latest > pkgVersion pkgId
                  then cont (Hackage name (Just latest)) -- Yes it is.
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
                         | otherwise -> downgradeRejected path ver pkgId
                  Just Hackage.Deprecated ->
                    case ver `compare` pkgVersion pkgId of
                      GT | optForce  -> deprUpdateForced path ver >> cont pkgURI
                      GT | otherwise -> deprUpdateRejected path ver
                      EQ             -> alreadyExactDepr path ver
                      LT | optForce  -> deprDowngradeForced path ver pkgId >> cont pkgURI
                         | otherwise -> deprDowngradeRejected path ver pkgId
                  Nothing ->
                    unavailable path ver

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

downgradeRejected :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
downgradeRejected path ver pkgId =
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

deprUpdateRejected :: HasCallStack => PosixPath -> Version -> CLI ()
deprUpdateRejected path ver =
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

deprDowngradeRejected :: HasCallStack => PosixPath -> Version -> PackageIdentifier -> CLI ()
deprDowngradeRejected path ver pkgId =
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
examineNewMeta (UpdateOptions {..}) pkg pkgId pkgURI =
  do path     <- pkgPath
     oldFlags <- packageFlags pkg
     -- This is new metadata. Package flags on the command line should be
     -- merged into old ones found in Makefile. The former should have a
     -- higher precedence over the latter.
     newMeta  <- withPkgFlagsModified (<> oldFlags)
                 $ fetchMeta pkgURI
     let cont = examineOldMeta pkg oldFlags newMeta
     case pkgURI of
       Hackage _ _ -> cont
       _ ->
         let ver = distVersion newMeta
         in
           case ver `compare` pkgVersion pkgId of
             GT             -> cont -- It's newer.
             EQ             -> alreadyLatest path pkgId
             -- It's older but the user explicitly asked to do it.
             LT | optForce  -> downgradeForced path ver pkgId >> cont
                | otherwise -> downgradeRejected path ver pkgId

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
               => SrcDb.Package CLI
               -> FlagMap
               -> PackageMeta
               -> CLI ()
examineOldMeta pkg oldFlags newMeta =
  do path    <- pkgPath
     isGit   <- (isJust .) . (<|>)
                <$> SrcDb.githubProject pkg
                <*> SrcDb.gitlabProject pkg
     -- As this is old metadata, no package flags on the command line
     -- arguments should be taken account of.
     oldMeta <-
       withPkgFlagsHidden . withPkgFlagsModified (const oldFlags) $
       if isGit then
         -- If it's from GitHub or GitLab, we can't construct the actual
         -- distfile URL by simply concatenating MASTER_SITES, DISTNAME,
         -- and EXTRACT_SUFX but it's clear it's not from Hackage anyway.
         fetchDistFile
       else
         do ms <- SrcDb.masterSites pkg
            case ms of
              [] ->
                fatal $ PP.hsep [ prettyAnsi path
                                , "has no MASTER_SITES."
                                , command mempty
                                , "does not know how to update this package."
                                ]
              (m:_) ->
                do distName  <- OP.decodeUtf =<< SrcDb.distName pkg
                   sufx      <- OP.decodeUtf =<< SrcDb.extractSufx pkg
                   oldPkgURI <- parsePackageURI Nothing $
                                m & uriPathLens %~ \base -> base <> distName <> sufx
                   if isFromHackage oldPkgURI
                     then fetchMeta oldPkgURI
                     else fetchDistFile
     applyChanges oldMeta newMeta
  where
    fetchDistFile :: CLI PackageMeta
    fetchDistFile =
      do runMake ["fetch"]
         distDir    <- SrcDb.distDir =<< srcDb
         distSubDir <- SrcDb.distSubDir pkg
         distName   <- SrcDb.distName pkg
         sufx       <- SrcDb.extractSufx pkg
         path       <- OP.decodeUtf
                       $ maybe distDir (distDir </>) distSubDir </> distName <> sufx
         fetchMeta (File path)

-- |This is a continuation of 'examineOldMeta'. Perform a 3-way merge based
-- on the current set of files and the old and new package metadata.
applyChanges :: HasCallStack
             => PackageMeta
             -> PackageMeta
             -> CLI ()
applyChanges oldMeta newMeta =
  error "FIXME: not impl"

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
