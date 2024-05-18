{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, category, debug, info, maintainer)
import Cabal2Pkg.Generator (summariseCabal)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Data.Text (Text)
import Text.Show.Pretty (ppShow)


run :: Text -> CLI ()
run url =
  do info ("Reading " <> url <> " ...")
     cabal <- readCabal url
     debug ("Found a package:\n" <> ppShow cabal)

     cat <- category
     mtr <- maintainer
     let meta = summariseCabal cat mtr cabal
     debug ("Summarised package metadata:\n" <> ppShow meta)
