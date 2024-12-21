{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), fatal, info, pkgPath, srcDb )
import Cabal2Pkg.Command.Common (fetchMeta)
import Cabal2Pkg.Hackage (AvailableVersions(AV), fetchHackageVersions)
import Cabal2Pkg.PackageURI (PackageURI(Hackage), isFromHackage, parsePackageURI)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Types.PackageId (PackageIdentifier(pkgName))
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP
import System.OsPath qualified as OP

run :: HasCallStack => UpdateOptions -> CLI ()
run (UpdateOptions {..}) =
  do -- Before doing anything expensive, see if files have conflict
     -- markers. Abort if there are any.
     -- FIXME: do it

     -- Look the package up in the source database. We need to know things
     -- like its current version.
     path <- pkgPath
     info ( "Examining the current state of" <+>
            PP.annotate (PP.colorDull PP.Green) (PP.pretty path) )

     db   <- srcDb
     pkg  <- do mpkg <- SrcDb.findPackageByPath db path
                case mpkg of
                  Just pkg -> pure pkg
                  Nothing  ->
                    fatal ( PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.pretty path)) <+>
                            "doesn't look like a pkgsrc package. It has no" <+>
                            PP.dquotes (PP.annotate (PP.color PP.Cyan) "Makefile") <>
                            PP.pretty '.' )

     -- Obtain the package metadata of the requested version (or the latest
     -- one). If it isn't newer than PKGVERSION_NOREV, then it's clear we
     -- can bail out now.
     --
     -- FIXME: If the user just requests the latest version, then it's
     -- faster to see if there's actually an update via
     -- https://hackage.haskell.org/api#versions before summarising the
     -- entire package description.
     pkgId   <- packageId pkg
     pkgURI  <- maybe (pure $ Hackage (pkgName pkgId) Nothing)
                      (parsePackageURI (Just $ pkgName pkgId))
                      optPackageURI
     if isFromHackage pkgURI
       then do AV vers <- fetchHackageVersions (pkgName pkgId)
               error (show vers)
       else pure ()
     newMeta <- fetchMeta pkgURI
     fail "FIXME: compare versions"

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

-- Reinterpret DISTNAME as a Cabal package identifier. They must be
-- identical.
packageId :: (MonadThrow m, MonadUnliftIO m)
          => SrcDb.Package m
          -> m PackageIdentifier
packageId pkg =
  do distName <- SrcDb.distName pkg
     dnStr    <- OP.decodeUtf distName
     case eitherParsec dnStr of
       Right pkgId -> pure pkgId
       Left  e     -> fatal (PP.viaShow e)
