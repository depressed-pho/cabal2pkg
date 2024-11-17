{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, InitOptions(..), debug, fatal, info)
import Cabal2Pkg.Extractor (hasLibraries, hasExecutables, summariseCabal)
import Cabal2Pkg.Generator.Buildlink3 (genBuildlink3)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Control.Exception.Safe (catch, throw)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import System.IO (Handle, hClose)
import System.IO.Error (isAlreadyExistsError)
import System.OsPath ((</>), OsPath)
import System.OsPath qualified as OP
import System.OsPath.Posix (PosixString)
import System.OsString.Internal.Types (OsString(..))
import System.Posix.IO.PosixString
  ( OpenFileFlags(..), OpenMode(..), defaultFileFlags, fdToHandle, openFd )
import Text.Show.Pretty (ppShow)

run :: HasCallStack => InitOptions -> CLI ()
run (InitOptions {..})
  = do info $ "Reading " <> optTarballURL <> " ..."
       cabal <- readCabal optTarballURL
       debug $ "Found a package:\n" <> T.pack (ppShow cabal)

       meta <- summariseCabal cabal
       debug $ "Summarised package metadata:\n" <> T.pack (ppShow meta)

       -- If the package only provides libraries but no executables,
       --   * It should have buildlink3.mk.
       --   * PKGNAME should have a prefix "hs-".
       --
       -- If it only provides executables but no libraries,
       --   * It shouldn't have buildlink3.mk.
       --   * PKGNAME shouldn't have a prefix "hs-".
       --
       -- If it provides both,
       --   * No rules as to whether to have buildlink3.mk. We generate one
       --     anyway, and let the user decide if they want to keep it.
       --   * No rules as to whether to have a prefix "hs-".
       --
       {-
       if hasLibraries meta
         then if hasExecutables meta
              then
-}

       --dir   <- pkgPath
       --mkOut <- open' (dir </> "Makefile")

       let mk = genMakefile meta
       debug $ "Generated Makefile:\n" <> TL.toStrict mk

       let bl3 = genBuildlink3 meta
       debug $ "Generated buildlink3.mk:\n" <> TL.toStrict bl3
  where
    open' :: OsPath -> CLI Handle
    open' path
      = openFileForWrite' path
        `catch`
        \(e :: IOError) ->
          if isAlreadyExistsError e
          then do path' <- T.pack <$> OP.decodeUtf path
                  fatal ("The file `" <> path' <> "' already exists. If you" <>
                         " want to overwrite it, re-run cabal2pkg with -w")
          else throw e

    openFileForWrite' :: MonadResource m => OsPath -> m Handle
    openFileForWrite'
      | optOverwrite = openFileForWrite
      | otherwise    = openFreshFile

-- |Open a file for writing. Existing files will be overwritten.
openFileForWrite :: MonadResource m => OsPath -> m Handle
openFileForWrite path
  = openFileHandle path WriteOnly flags
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags
            { exclusive = False
            , trunc     = True
            , creat     = Just 0o666
            , cloexec   = True
            }

-- |Open a file for writing, but only when the file doesn't already
-- exist. If it exists the action raises an 'IOError'.
openFreshFile :: MonadResource m => OsPath -> m Handle
openFreshFile path
  = openFileHandle path WriteOnly flags
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags
            { exclusive = True
            , trunc     = True
            , creat     = Just 0o666
            , cloexec   = True
            }

openFileHandle :: MonadResource m => OsPath -> OpenMode -> OpenFileFlags -> m Handle
openFileHandle path mode flags
  = snd <$> allocate (fdToHandle =<< openFd path' mode flags) hClose
  where
    path' :: PosixString
    path' = getOsString path
