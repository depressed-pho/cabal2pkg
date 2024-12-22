{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), fatal, info, warn, pkgPath, srcDb )
import Cabal2Pkg.Command.Common (command, option, fetchMeta)
import Cabal2Pkg.Hackage qualified as Hackage
import Cabal2Pkg.PackageURI (PackageURI(..), isFromHackage, parsePackageURI)
import Cabal2Pkg.Pretty (prettyAnsi)
import Control.Exception.Safe (MonadThrow, assert)
import Control.Monad (unless)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map.Strict qualified as M
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Types.PackageId (PackageIdentifier(pkgName, pkgVersion))
import Distribution.Types.Version (Version)
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OP

run :: HasCallStack => UpdateOptions -> CLI ()
run (UpdateOptions {..}) =
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
                    fatal ( prettyAnsi path <+>
                            "doesn't look like a pkgsrc package. It has no" <+>
                            prettyAnsi [pstr|Makefile|] <>
                            PP.pretty '.' )

     -- If the user requested a version from Hackage, we can take a fast
     -- route getting the set of available versions and see if there's
     -- actually an update.
     pkgId  <- packageId pkg
     pkgURI <- maybe (pure $ Hackage (pkgName pkgId) Nothing)
                     (parsePackageURI (Just $ pkgName pkgId))
                     optPackageURI
     case pkgURI of
       HTTP _ -> fail "FIXME: not impl"
       File _ -> fail "FIXME: not impl"
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
                  then fail "FIXME: not impl" -- Yes it is.
                  else alreadyLatest path pkgId
              Just ver ->
                -- The user requested a specific version. Does it exist?
                -- Is it any newer then the current one? Is it preferred?
                case M.lookup ver (Hackage.unAV av) of
                  Just Hackage.Normal ->
                    case ver `compare` pkgVersion pkgId of
                      GT             -> fail "FIXME: not impl" -- Yes it is.
                      EQ             -> alreadyExact path ver
                      -- It's not but the user explicitly asked to do it.
                      LT | optForce  -> downgradeForced path ver pkgId
                         | otherwise -> downgradeRejected path ver pkgId
                  Just Hackage.Deprecated ->
                    case ver `compare` pkgVersion pkgId of
                      GT | optForce  -> deprUpdateForced path ver
                      GT | otherwise -> deprUpdateRejected path ver
                      EQ             -> alreadyExactDepr path ver
                      LT | optForce  -> deprDowngradeForced path ver pkgId
                         | otherwise -> deprDowngradeRejected path ver pkgId
                  Nothing ->
                    unavailable path ver

alreadyLatest :: PosixPath -> PackageIdentifier -> CLI ()
alreadyLatest path pkgId =
  info $ PP.hsep [ prettyAnsi path
                 , "is at version"
                 , prettyAnsi (pkgVersion pkgId)
                 , "but it's already the latest one."
                 ]

alreadyExact :: PosixPath -> Version -> CLI ()
alreadyExact path ver =
  info $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's already at that exact version."
                 ]

downgradeForced :: PosixPath -> Version -> PackageIdentifier -> CLI ()
downgradeForced path ver pkgId =
  do warn $ PP.hsep [ "You requested to update"
                    , prettyAnsi path
                    , "to version"
                    , prettyAnsi ver
                    , "but it's older than the current version"
                    , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                    , "Proceeding to downgrade it anyway because you explicitly asked to."
                    ]
     fail "FIXME: not impl"

downgradeRejected :: PosixPath -> Version -> PackageIdentifier -> CLI ()
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

deprUpdateForced :: PosixPath -> Version -> CLI ()
deprUpdateForced path ver =
  do warn $ PP.hsep [ "You requested to update"
                    , prettyAnsi path
                    , "to version"
                    , prettyAnsi ver
                    , "but it's been marked as deprecated on Hackage,"
                    , "which usually means it has known defects."
                    , "Proceeding to use this version anyway because you explicitly asked to."
                    ]
     fail "FIXME: not impl"

deprUpdateRejected :: PosixPath -> Version -> CLI ()
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

alreadyExactDepr :: PosixPath -> Version -> CLI ()
alreadyExactDepr path ver =
  warn $ PP.hsep [ "You requested to update"
                 , prettyAnsi path
                 , "to version"
                 , prettyAnsi ver
                 , "but it's already at that exact version."
                 , "Note that it's been marked as deprecated on Hackage,"
                 , "which usually means it has known defects."
                 ]

deprDowngradeForced :: PosixPath -> Version -> PackageIdentifier -> CLI ()
deprDowngradeForced path ver pkgId =
  do warn $ PP.hsep [ "You requested to update"
                    , prettyAnsi path
                    , "to version"
                    , prettyAnsi ver
                    , "but it's older than the current version"
                    , prettyAnsi (pkgVersion pkgId) <> PP.pretty '.'
                    , "The requested version is also marked as deprecated on Hackage,"
                    , "which usually means it has known defects."
                    , "Proceeding to downgrade it to this version anyway because you explicitly asked to."
                    ]
     fail "FIXME: not impl"

deprDowngradeRejected :: PosixPath -> Version -> PackageIdentifier -> CLI ()
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

unavailable :: PosixPath -> Version -> CLI ()
unavailable path ver =
  fatal $ PP.hsep [ "You requested to update"
                  , prettyAnsi path
                  , "to version"
                  , prettyAnsi ver
                  , "but this specific version is not available in Hackage."
                  ]
{-
     -- Obtain the package metadata of the requested version (or the latest
     -- one). If it isn't newer than PKGVERSION_NOREV, then it's clear we
     -- can bail out now.

     -- Obtain the package metadata of the current version somehow. If it's
     -- in Hackage we can just query the Hackage API using its DISTNAME,
     -- because DISTNAME is identical to the package ID. But if not... we
     -- first need to run "make fetch" and try to locate a tarball in
     -- "${DISTDIR}/${DIST_SUBDIR}".
     --
     -- We could just do the latter all the time, which would be fine if we
     -- have already downloaded the tarball. Otherwise we would end up
     -- downloading one that is soon going to be useless. A single .cabal
     -- file is almost always smaller than the entire tarball even if it's
     -- uncompressed so...

     dd <- SrcDb.distDir db
     info (PP.viaShow dd)

     ms <- SrcDb.masterSites pkg
     info (PP.viaShow ms)

     es <- SrcDb.extractSufx pkg
     info (PP.viaShow es)

     rev <- SrcDb.pkgRevision pkg
     info (PP.viaShow rev)

     error "FIXME"
-}

-- |Reinterpret DISTNAME as a Cabal package identifier. This is guaranteed
-- to be a valid interpretation as long as the package Makefile includes
-- @mk/haskell.mk@.
packageId :: (MonadThrow m, MonadUnliftIO m)
          => SrcDb.Package m
          -> m PackageIdentifier
packageId pkg =
  do isHask <- SrcDb.includesHaskellMk pkg
     unless isHask $
       let path = SrcDb.pkgPath pkg
       in
         fatal ( prettyAnsi path <+>
                 "doesn't look like a Haskell package. It doesn't include" <+>
                 prettyAnsi [pstr|mk/haskell.mk|] <>
                 PP.pretty '.' )
     distName <- SrcDb.distName pkg
     dnStr    <- OP.decodeUtf distName
     case eitherParsec dnStr of
       Right pkgId -> pure pkgId
       Left  e     -> fatal (PP.viaShow e)
