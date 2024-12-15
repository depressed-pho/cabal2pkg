{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Update
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, UpdateOptions(..), fatal, pkgPath, srcDb )
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
     db   <- srcDb
     pkg  <- do mpkg <- SrcDb.findPackageByPath db path
                case mpkg of
                  Just pkg -> pure pkg
                  Nothing  ->
                    fatal ( PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.pretty path)) <+>
                            "doesn't look like a pkgsrc package. It has no" <+>
                            PP.dquotes (PP.annotate (PP.color PP.Cyan) "Makefile") <>
                            PP.pretty '.' )
     error "FIXME"
