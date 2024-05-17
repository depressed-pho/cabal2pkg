{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Cabal2Pkg.CmdLine
  ( CLI
  , Command(..)

  , runCLI
  , command
  , maintainer

    -- * Message output
  , debug
  , info
  ) where

import Cabal2Pkg.Utils qualified as Utils
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
  ( (<**>), Parser, ReadM, argument
  , eitherReader, execParser, fullDesc, header, help, helper, long
  , metavar, option, progDesc, short, showDefault, str, subparser, switch
  , value
  )
import Options.Applicative qualified as OA
import System.Environment (lookupEnv)
import System.IO (utf8, utf16le)
import System.OsPath qualified as OP
import System.OsPath (OsPath)


newtype CLI a = CLI { unCLI :: ReaderT Options (ResourceT IO) a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadFix
           , MonadIO
           , MonadResource
           , MonadThrow
           , PrimMonad
           )

instance MonadFail CLI where
  fail :: String -> CLI a
  fail = CLI . fail . ("ERROR: " <>)

runCLI :: CLI a -> IO a
runCLI m
  = do opts <- getOptions
       runResourceT $ runReaderT (unCLI m) opts

command :: CLI Command
command = CLI $ optCommand <$> ask

maintainer :: CLI (Maybe Text)
maintainer =
  do m0 <- liftIO $ lookupEnv "PKGMAINTAINER"
     m1 <- liftIO $ lookupEnv "REPLYTO"
     pure $ T.pack <$> (m0 <|> m1)

debug :: String -> CLI ()
debug msg =
  do d <- CLI $ optDebug <$> ask
     when d $
       liftIO $ putStrLn msg

info :: String -> CLI ()
info = liftIO . putStrLn


data Options
  = Options
    { optCommand   :: Command
    , optDebug     :: Bool
    , optDirectory :: OsPath
    }
  deriving (Show)

optionsP :: Parser Options
optionsP =
  Options
  <$> commandP
  <*> switch
      ( long "debug" <>
        short 'd' <>
        help "Show debugging output only useful for development of cabal2pkg"
      )
  <*> option path
      ( long "pkgpath" <>
        short 'p' <>
        help "The path to the pkgsrc package to work with" <>
        showDefault <>
        value (Utils.unsafeEncodeUtf ".") <>
        metavar "DIR"
      )

path :: ReadM OsPath
path = eitherReader f
  where
    f :: String -> Either String OsPath
    f = first show . OP.encodeWith utf8 utf16le


data Command
  = Init { initTarballURL :: String }
  | Update
  deriving (Show)

commandP :: Parser Command
commandP =
  subparser
  ( OA.command "init" (OA.info initP (progDesc "Create a new pkgsrc package")) <>
    OA.command "update" (OA.info updateP (progDesc "Update an existing pkgsrc package to the latest version"))
  )
  where
    initP   = Init <$> argument str (metavar "TARBALL-URL")
    updateP = pure Update


getOptions :: IO Options
getOptions = execParser opts
  where
    opts = OA.info (optionsP <**> helper)
           ( fullDesc
             <> header "cabal2pkg - a tool to automate importing Cabal packages to pkgsrc"
           )
