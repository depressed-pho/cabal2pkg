{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, debug, info)
import Cabal2Pkg.Extractor (summariseCabal)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

run :: HasCallStack => Text -> CLI ()
run url
  = do info $ "Reading " <> url <> " ..."
       cabal <- readCabal url
       debug $ "Found a package:\n" <> T.pack (ppShow cabal)

       meta <- summariseCabal cabal
       debug $ "Summarised package metadata:\n" <> T.pack (ppShow meta)

       let mk = genMakefile meta
       debug $ "Generated Makefile:\n" <> TL.toStrict mk
