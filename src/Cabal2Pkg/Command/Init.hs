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
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import Data.Text.Lazy qualified

run :: HasCallStack => Text -> CLI ()
run url =
  do info $ "Reading " <> url <> " ..."
     cabal <- readCabal url
     debug $ "Found a package:\n" <> T.pack (ppShow cabal)

     cat <- category
     mtr <- maintainer
     let meta = summariseCabal cat mtr cabal
     debug $ "Summarised package metadata:\n" <> T.pack (ppShow meta)

     mk  <- genMakefile meta
     debug $ "Generated Makefile:\n" <> Data.Text.Lazy.toStrict mk
