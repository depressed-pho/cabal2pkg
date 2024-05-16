{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Cabal2Pkg.CmdLine (getOptions)


main ∷ IO ()
main = print =<< getOptions
