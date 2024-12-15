{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), fatal, info, pkgPath, srcDb )
import Database.Pkgsrc.SrcDb qualified as SrcDb
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal qualified as PP

run :: HasCallStack => UpdateOptions -> CLI ()
run (UpdateOptions {..}) =
  do -- First, look the package up in the source database. We need to know
     -- things like its current version.
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

     -- Next, obtain the package metadata of the current version
     -- somehow. If it's in Hackage we can just query the Hackage API using
     -- its DISTNAME, because DISTNAME is equivalent to the package ID. But
     -- if not... we first need to run "make fetch" and try to locate a
     -- tarball in ${DISTDIR}/${DIST_SUBDIR}.
     --
     -- We could just do the latter all the time, which would be fine if we
     -- have already downloaded the tarball. Otherwise we would end up
     -- downloading one that is soon going to be useless. A single .cabal
     -- file is almost always smaller than the entire tarball even if it's
     -- uncompressed so...

     ms <- SrcDb.masterSites pkg
     info (PP.viaShow ms)

     es <- SrcDb.extractSufx pkg
     info (PP.viaShow es)

     rev <- SrcDb.pkgRevision pkg
     info (PP.viaShow rev)
