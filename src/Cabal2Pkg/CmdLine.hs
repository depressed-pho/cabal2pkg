{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-} -- for deriving Exception
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Cabal2Pkg.CmdLine
  ( CLI
  , FlagMap
  , Command(..)
  , CommandError(..)
  , InitOptions(..)
  , UpdateOptions(..)

  , runCLI
  , command
  , pkgPath
  , category
  , maintainer
  , pkgFlags
  , progDb
  , ghcVersion
  , installedPkgs
  , srcDb

    -- * Message output
  , debug
  , info
  , warn
  , fatal
  ) where

import Cabal2Pkg.Static (makeQ)
import Control.Applicative ((<|>), many)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Exception.Safe (Exception(..), catch, throw)
import Control.Monad (foldM, unless, when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Bifunctor (first, second)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Database.Pkgsrc.SrcDb (SrcDb, createSrcDb)
import Distribution.Parsec (explicitEitherParsec)
import Distribution.Simple.Compiler qualified as C
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program.Builtin qualified as C
import Distribution.Simple.Program.Db (ProgramDb)
import Distribution.Simple.Program.Db qualified as C
import Distribution.Simple.Program.Types qualified as C
import Distribution.Types.Flag (FlagName)
import Distribution.Types.Flag qualified as C
import Distribution.Types.Version (Version)
import Distribution.Verbosity (silent)
import GHC.Paths qualified as Paths
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
import System.OsPath.IsString ()
import Text.Show.Pretty (ppShow)


type FlagMap = Map FlagName Bool

data Options
  = Options
    { optCommand  :: !Command
    , optDebug    :: !Bool
    , optPkgPath  :: !OsPath
    , optPkgFlags :: !FlagMap
    , optGHCBin   :: !OsPath
    , optMakeBin  :: !OsPath
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
        value (fromString Paths.ghc) <>
        metavar "FILE"
      )
  <*> option path
      ( long "make" <>
        help "The path to the BSD make(1) command" <>
        showDefault <>
        value $$makeQ <>
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
    f = second fa2Map . explicitEitherParsec C.legacyParsecFlagAssignment

    fa2Map :: C.FlagAssignment -> FlagMap
    fa2Map = M.fromList . C.unFlagAssignment

data Command
  = Init   !InitOptions
  | Update !UpdateOptions
  deriving (Show)

data InitOptions
  = InitOptions
    { optOverwrite  :: !Bool
    , optTarballURL :: !Text
    }
  deriving (Show)

data UpdateOptions
  = UpdateOptions
    {
    }
  deriving (Show)

commandP :: Parser Command
commandP =
  subparser
  ( OA.command "init"   (OA.info initP   (progDesc "Create a new pkgsrc package")) <>
    OA.command "update" (OA.info updateP (progDesc "Update an existing pkgsrc package to the latest version"))
  )
  where
    initP :: Parser Command
    initP = (Init .) . InitOptions
            <$> switch
                ( long "overwrite" <>
                  short 'w' <>
                  help "Overwrite existing files"
                )
            <*> argument str (metavar "TARBALL-URL")

    updateP :: Parser Command
    updateP = pure $ Update UpdateOptions


parseOptions :: IO Options
parseOptions =
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
              fatal (dir' <> " doesn't look like a pkgsrc package directory")
         pure $ opts { optPkgPath = dir }


data Context
  = Context
    { ctxOptions :: !Options
    , ctxProgDb  :: !(Deferred CLI ProgramDb)
    , ctxIPI     :: !(Deferred CLI InstalledPackageIndex)
    , ctxSrcDb   :: !(Deferred CLI (SrcDb CLI))
    }

initialCtx :: (MonadThrow m, MonadUnliftIO m) => Options -> m Context
initialCtx opts
  = do progs <- defer mkProgDb
       ipi   <- defer readPkgDb
       sdb   <- defer mkSrcDb
       pure Context
         { ctxOptions = opts
         , ctxProgDb  = progs
         , ctxIPI     = ipi
         , ctxSrcDb   = sdb
         }

mkProgDb :: CLI ProgramDb
mkProgDb
  = do debug "Configuring program database..."
       ghcBin      <- OP.decodeUtf . optGHCBin =<< options
       (_, _, db0) <- liftIO $
                      GHC.configure silent (Just ghcBin) Nothing C.defaultProgramDb
       db1         <- liftIO $
                      foldM (flip $ C.configureProgram silent) db0 bundledProgs
       debug . T.pack $ ppShow db1
       pure db1
  where
    -- We hate to hard-code these, but there seems to be no ways to avoid
    -- it.
    bundledProgs :: [C.Program]
    bundledProgs = [ C.haddockProgram
                   , C.hsc2hsProgram
                   ]

readPkgDb :: CLI InstalledPackageIndex
readPkgDb
  = do debug "Reading installed package index..."
       progs <- progDb
       liftIO $ GHC.getPackageDBContents silent C.GlobalPackageDB progs

mkSrcDb :: CLI (SrcDb CLI)
mkSrcDb
  = do make <- optMakeBin <$> options
       root <- OP.takeDirectory . OP.takeDirectory <$> pkgPath
       createSrcDb make root


newtype CommandError = CommandError { message :: Text }
  deriving stock    Show
  deriving anyclass Exception

newtype CLI a = CLI { unCLI :: ReaderT Context (ResourceT IO) a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadCatch
                   , MonadFix
                   , MonadIO
                   , MonadResource
                   , MonadThrow
                   , MonadUnliftIO
                   , PrimMonad
                   )

instance MonadFail CLI where
  fail :: String -> CLI a
  fail = fatal . T.pack

instance Monoid a => Monoid (CLI a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (CLI a) where
  (<>) = liftA2 (<>)

runCLI :: CLI a -> IO a
runCLI m =
  ( do opts <- parseOptions
       ctx  <- initialCtx opts
       runResourceT (runReaderT (unCLI m) ctx)
  )
  `catch`
  \(e :: CommandError) ->
    do pn <- T.pack <$> getProgName
       hPutStrLn stderr (pn <> ": ERROR: " <> message e)
       exitWith (ExitFailure 1)

options :: CLI Options
options = CLI $ asks ctxOptions

command :: CLI Command
command = optCommand <$> options

pkgPath :: CLI OsPath
pkgPath = optPkgPath <$> options

-- |@-d devel/foo@ => @devel@
category :: CLI Text
category = toText . OP.takeFileName . OP.takeDirectory =<< pkgPath
  where
    toText :: MonadThrow m => OsPath -> m Text
    toText = (T.pack <$>) . OP.decodeUtf

maintainer :: CLI (Maybe Text)
maintainer =
  do m0 <- liftIO $ lookupEnv "PKGMAINTAINER"
     m1 <- liftIO $ lookupEnv "REPLYTO"
     pure $ T.pack <$> (m0 <|> m1)

pkgFlags :: CLI FlagMap
pkgFlags = optPkgFlags <$> options

progDb :: CLI ProgramDb
progDb = CLI (asks ctxProgDb) >>= force

ghcVersion :: CLI Version
ghcVersion
  = do progs <- progDb
       let ghc  = fromJust $ C.lookupKnownProgram "ghc" progs
           ghc' = fromJust $ C.lookupProgram ghc progs
           ver  = fromJust $ C.programVersion ghc'
       pure ver

installedPkgs :: CLI InstalledPackageIndex
installedPkgs = CLI (asks ctxIPI) >>= force

srcDb :: CLI (SrcDb CLI)
srcDb = CLI (asks ctxSrcDb) >>= force

debug :: Text -> CLI ()
debug msg =
  do d <- optDebug <$> options
     when d . liftIO $
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

fatal :: MonadThrow m => Text -> m a
fatal = throw . CommandError
