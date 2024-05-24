{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Cabal2Pkg.CmdLine
  ( CLI
  , FlagMap
  , Command(..)
  , CommandError(..)

  , runCLI
  , command
  , pkgPath
  , category
  , maintainer
  , pkgFlags
  , ghcVersion

    -- * Message output
  , debug
  , info
  , warn
  , err
  ) where

import Cabal2Pkg.Utils ()
import Control.Applicative ((<|>), many)
import Control.Exception.Safe (Exception(..), catch, throw)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Bifunctor (first, second)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Distribution.Parsec (eitherParsec)
import Distribution.Types.Flag (FlagName)
import Distribution.Types.Flag qualified as C
import Distribution.Types.Version (Version)
import GHC.Paths (ghc)
import Options.Applicative
  ( (<**>), Parser, ReadM, argument, eitherReader, execParser, fullDesc, header
  , help, helper, long, metavar, option, progDesc, short, showDefault, str
  , subparser, switch , value
  )
import Options.Applicative qualified as OA
import System.Directory.OsPath (doesFileExist, canonicalizePath)
import System.Environment (getProgName, lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (stderr, utf8, utf16le)
import System.OsPath qualified as OP
import System.OsPath ((</>), OsPath)
import System.Process (readProcess)


type FlagMap = Map FlagName Bool

data Options
  = Options
    { optCommand  :: !Command
    , optDebug    :: !Bool
    , optPkgPath  :: !OsPath
    , optPkgFlags :: !FlagMap
    , optGHCBin   :: !OsPath
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
        value "." <>
        metavar "DIR"
      )
  <*> ( M.unions <$> many
        ( option flagMap
          ( long "flag" <>
            short 'f' <>
            help "Cabal package flags, such as \"+foo -bar\"" <>
            metavar "FLAG"
          )
        )
      )
  <*> option path
      ( long "ghc" <>
        help "The path to the GHC executable" <>
        showDefault <>
        value (fromString ghc) <>
        metavar "FILE"
      )

path :: ReadM OsPath
path = eitherReader f
  where
    f :: String -> Either String OsPath
    f = first show . OP.encodeWith utf8 utf16le

flagMap :: ReadM FlagMap
flagMap = eitherReader f
  where
    f :: String -> Either String FlagMap
    f = second fa2Map . eitherParsec

    fa2Map :: C.FlagAssignment -> FlagMap
    fa2Map = M.fromList . C.unFlagAssignment


data Command
  = Init { initTarballURL :: Text }
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
getOptions =
  do opts <- execParser spec
     validatePkgPath opts
  where
    spec = OA.info (optionsP <**> helper)
           ( fullDesc
             <> header "cabal2pkg - a tool to automate importing Cabal packages to pkgsrc"
           )

    validatePkgPath :: Options -> IO Options
    validatePkgPath opts =
      do dir  <- canonicalizePath $ optPkgPath opts
         -- Does it look like a package directory?
         p    <- doesFileExist (dir </> ".." </> ".." </> "mk" </> "bsd.pkg.mk")
         unless p $
           do dir' <- T.pack <$> OP.decodeUtf dir
              err (dir' <> " doesn't look like a pkgsrc package directory")
         pure $ opts { optPkgPath = dir }


data CommandError = CommandError { message :: Text }
  deriving (Show, Exception)

newtype CLI a = CLI { unCLI :: ReaderT Options (ResourceT IO) a }
  deriving newtype ( Applicative
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
  fail = err . T.pack

runCLI :: CLI a -> IO a
runCLI m =
  ( do opts <- getOptions
       runResourceT (runReaderT (unCLI m) opts)
  )
  `catch`
  \(e :: CommandError) ->
    do pn <- T.pack <$> getProgName
       hPutStrLn stderr (pn <> ": ERROR: " <> message e)
       exitWith (ExitFailure 1)

command :: CLI Command
command = CLI $ asks optCommand

pkgPath :: CLI OsPath
pkgPath = CLI $ asks optPkgPath

-- |@-d devel/foo@ => @devel@
category :: CLI Text
category = toText =<< OP.takeFileName . OP.takeDirectory <$> pkgPath
  where
    toText :: MonadThrow m => OsPath -> m Text
    toText = (T.pack <$>) . OP.decodeUtf

maintainer :: CLI (Maybe Text)
maintainer =
  do m0 <- liftIO $ lookupEnv "PKGMAINTAINER"
     m1 <- liftIO $ lookupEnv "REPLYTO"
     pure $ T.pack <$> (m0 <|> m1)

pkgFlags :: CLI FlagMap
pkgFlags = CLI $ asks optPkgFlags

ghcVersion :: CLI Version
ghcVersion
  = do ghcBin <- OP.decodeUtf =<< CLI (asks optGHCBin)
       verStr <- liftIO $ readProcess ghcBin ["--numeric-version"] ""
       either (err . T.pack) pure $ eitherParsec (trim verStr)
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

debug :: Text -> CLI ()
debug msg =
  do d <- CLI $ asks optDebug
     when d $ liftIO $
       do pn <- T.pack <$> getProgName
          hPutStrLn stderr (pn <> ": DEBUG: " <> T.strip msg)

info :: MonadIO m => Text -> m ()
info msg =
  liftIO $
  do pn <- T.pack <$> getProgName
     hPutStrLn stderr (pn <> ": " <> T.strip msg)

warn :: MonadIO m => Text -> m ()
warn msg =
  liftIO $
  do pn <- T.pack <$> getProgName
     hPutStrLn stderr (pn <> ": WARNING: " <> T.strip msg)

err :: MonadThrow m => Text -> m a
err = throw . CommandError
