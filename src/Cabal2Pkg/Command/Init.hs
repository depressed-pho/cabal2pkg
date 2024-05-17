{-# LANGUAGE ScopedTypeVariables #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, debug, info, maintainer)
import Cabal2Pkg.Generator (summariseCabal)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Text.Show.Pretty (ppShow)


run :: String -> CLI ()
run url =
  do info ("Reading " <> url <> " ...")
     cabal <- readCabal url
     debug ("Found a package:\n" <> ppShow cabal)

     mtr <- maintainer
     let meta = summariseCabal mtr cabal
     debug ("Summarised package metadata:\n" <> ppShow meta)
