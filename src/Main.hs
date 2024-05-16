{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Cabal2Pkg.CmdLine
  ( Command(Init, Update), getOptions, optCommand, optDirectory )
import Cabal2Pkg.Command.Init qualified as Init
import Control.Monad.Trans.Resource (runResourceT)


main ∷ IO ()
main =
  do opts ← getOptions
     runResourceT $
       case optCommand opts of
         Init url →
           Init.run (optDirectory opts) url
         Update →
           fail "FIXME: not implemented yet"
