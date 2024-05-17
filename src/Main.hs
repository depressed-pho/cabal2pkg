module Main where

import Cabal2Pkg.CmdLine (Command(Init, Update), command, runCLI)
import Cabal2Pkg.Command.Init qualified as Init


main :: IO ()
main =
  runCLI $
  do cmd <- command
     case cmd of
         Init url ->
           Init.run url
         Update ->
           fail "FIXME: not implemented yet"
