module Main (main) where

import Cabal2Pkg.CmdLine (Command(..), command, runCLI)
import Cabal2Pkg.Command.Init qualified as Init
import Cabal2Pkg.Command.Update qualified as Update
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (setNumCapabilities, getNumProcessors)


main :: IO ()
main =
  runCLI $
  do numProc <- liftIO getNumProcessors
     liftIO . setNumCapabilities $ max 1 (numProc - 1)

     cmd <- command
     case cmd of
         Init opts ->
           Init.run opts
         Update opts ->
           Update.run opts
