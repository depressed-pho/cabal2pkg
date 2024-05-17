{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  ) where

import Cabal2Pkg.Utils (embedMustacheRelative)
import Text.Microstache (Template)


genMakefile :: Template
genMakefile = template

template :: Template
template = $$(embedMustacheRelative "templates/Makefile.mustache")
