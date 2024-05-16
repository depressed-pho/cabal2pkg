{-# LANGUAGE UnicodeSyntax #-}
module Cabal2Pkg.CmdLine
  ( Options(..)
  , Command(..)
  , getOptions
  )
  where

import Options.Applicative
  ( (<**>), Parser, argument, command, execParser, fullDesc, header, helper
  , info, metavar, progDesc, str, subparser
  )

data Options
  = Options
    { optCommand ∷ Command
    }
  deriving (Show)

optionsP ∷ Parser Options
optionsP = Options <$> commandP


data Command
  = Init { initTarballURL ∷ String }
  deriving (Show)

commandP ∷ Parser Command
commandP
  = subparser
    ( command "init" (info initP (progDesc "Create a new pkgsrc package"))
    )
  where
    initP
      = Init <$> argument str (metavar "TARBALL-URL")


getOptions ∷ IO Options
getOptions = execParser opts
  where
    opts = info (optionsP <**> helper)
           ( fullDesc
             <> header "cabal2pkg - a tool to automate importing Cabal packages to pkgsrc"
           )
