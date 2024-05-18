{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  ) where

import Cabal2Pkg.Generator (PackageMeta)
import Cabal2Pkg.Utils (embedMustacheRelative, renderMustacheE)
import Control.Exception.Safe (MonadThrow)
import Data.Text.Lazy qualified as LT
import GHC.Stack (HasCallStack)
import Text.Microstache (Template)


genMakefile :: (HasCallStack, MonadThrow m) => PackageMeta -> m LT.Text
genMakefile = renderMustacheE template

template :: Template
template = $$(embedMustacheRelative "templates/Makefile.mustache")
