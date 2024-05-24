{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, debug, info)
import Cabal2Pkg.Extractor (summariseCabal)
import Cabal2Pkg.Utils (embedEDERelative, renderEDE)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import Data.Text.Lazy qualified

run :: HasCallStack => Text -> CLI ()
run url
  = do info $ "Reading " <> url <> " ..."
       cabal <- readCabal url
       debug $ "Found a package:\n" <> T.pack (ppShow cabal)

       meta <- summariseCabal cabal
       debug $ "Summarised package metadata:\n" <> T.pack (ppShow meta)

       mk  <- genMakefile meta
       debug $ "Generated Makefile:\n" <> Data.Text.Lazy.toStrict mk
  where
    {-# NOINLINE makefile #-}
    makefile = $$(embedEDERelative "templates/Makefile.ede")
    genMakefile = renderEDE makefile
