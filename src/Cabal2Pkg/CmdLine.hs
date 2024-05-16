{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
module Cabal2Pkg.CmdLine
  ( Options(..)
  , Command(..)
  , getOptions
  )
  where

import Data.Bifunctor (first)
import Options.Applicative
  ( (<**>), Parser, ReadM, argument, command
  , eitherReader, execParser, fullDesc, header, help, helper, info, long
  , metavar, option, progDesc, short, showDefault, str, subparser, value
  )
import Prelude.Unicode ((∘))
import System.IO (utf8, utf16le)
#if !MIN_VERSION_filepath(1, 5, 2)
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.OsPath qualified as OP
import System.OsPath (OsPath)


data Options
  = Options
    { optCommand ∷ Command
    , optDirectory :: OsPath
    }
  deriving (Show)

optionsP ∷ Parser Options
optionsP
  = Options
    <$> commandP
    <*> option path
        ( long "directory" <>
          short 'd' <>
          help "The path to the pkgsrc package to work with" <>
          showDefault <>
          value (unsafeEncodeUtf ".") <>
          metavar "DIR"
        )

path ∷ ReadM OsPath
path = eitherReader f
  where
    f ∷ String → Either String OsPath
    f = first show ∘ OP.encodeWith utf8 utf16le

-- A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf ∷ String → OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = unsafePerformIO ∘ OP.encodeUtf
#endif

data Command
  = Init { initTarballURL ∷ String }
  | Update
  deriving (Show)

commandP ∷ Parser Command
commandP
  = subparser
    ( command "init" (info initP (progDesc "Create a new pkgsrc package")) <>
      command "update" (info updateP (progDesc "Update an existing pkgsrc package to the latest version"))
    )
  where
    initP   = Init <$> argument str (metavar "TARBALL-URL")
    updateP = pure Update


getOptions ∷ IO Options
getOptions = execParser opts
  where
    opts = info (optionsP <**> helper)
           ( fullDesc
             <> header "cabal2pkg - a tool to automate importing Cabal packages to pkgsrc"
           )
