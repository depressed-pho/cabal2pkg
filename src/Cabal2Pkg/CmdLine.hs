{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-} -- for deriving Exception
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Cabal2Pkg.CmdLine
  ( CLI
  , FlagMap
  , Command(..)
  , CommandError(..)
  , InitOptions(..)
  , UpdateOptions(..)

    -- * Running the CLI monad
  , runCLI

    -- * Query
  , command
  , origPkgPath
  , canonPkgPath
  , category
  , maintainer
  , makeCmd
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

    -- * Running @make(1)@
  , runMake
  ) where

import Cabal2Pkg.Static (makeQ)
import Control.Applicative ((<|>), (<**>), many)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Exception.Safe (Exception(..), MonadMask, catch, throw)
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
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
import GHC.Paths.OsPath qualified as Paths
import Options.Applicative (Parser, ParserInfo, ParserPrefs, ReadM)
import Options.Applicative qualified as OA
import PackageInfo_cabal2pkg qualified as PI
import Prelude hiding (print)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.Console.ANSI (hNowSupportsANSI)
import System.Directory.OsPath (doesFileExist, canonicalizePath)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (stderr, utf8, utf16le)
import System.OsPath qualified as OP
import System.OsPath ((</>), OsPath, osp)
import System.Process.Typed qualified as PT
import Text.Show.Pretty (ppShow)


type FlagMap = Map FlagName Bool

data Options
  = Options
    { optCommand  :: !Command
      -- ^'Nothing' denotes @auto@
    , optColour   :: !ColourPref
    , optDebug    :: !Bool
    , optPkgPath  :: !OsPath
    , optPkgFlags :: !FlagMap
    , optGHCCmd   :: !OsPath
    , optMakeCmd  :: !OsPath
    }
  deriving (Show)

optionsP :: Bool -> Parser Options
optionsP noColor =
  Options
  <$> commandP
  <*> OA.option colourPref
      ( OA.long "colour" <>
        OA.long "color" <>
        OA.help ("Use colours on output. WHEN can be \"never\", \"always\", or " <>
                 "\"auto\", where \"auto\" enables colours only when the stderr " <>
                 "is a terminal") <>
        OA.value (defaultColourPref noColor) <>
        OA.showDefault <>
        OA.metavar "WHEN"
      )
  <*> OA.switch
      ( OA.long "debug" <>
        OA.short 'd' <>
        OA.help ("Show debugging output that is only useful for developing " <>
                 PI.name)
      )
  <*> OA.option path
      ( OA.long "pkgpath" <>
        OA.short 'p' <>
        OA.help "The path to the pkgsrc package to work with" <>
        OA.showDefault <>
        OA.value [osp|.|] <>
        OA.metavar "DIR"
      )
  <*> ( M.unions <$> many
        ( OA.option flagMap
          ( OA.long "flag" <>
            OA.short 'f' <>
            OA.help "Cabal package flags, such as \"+foo -bar\"" <>
            OA.metavar "FLAG"
          )
        )
      )
  <*> OA.option path
      ( OA.long "ghc" <>
        OA.help "The path to the GHC executable" <>
        OA.showDefault <>
        OA.value Paths.ghc <>
        OA.metavar "FILE"
      )
  <*> OA.option path
      ( OA.long "make" <>
        OA.help "The path to the BSD make(1) command" <>
        OA.showDefault <>
        OA.value $$makeQ <>
        OA.metavar "FILE"
      )

data ColourPref = Always | Never | Auto

instance Show ColourPref where
  show Always = "always"
  show Never  = "never"
  show Auto   = "auto"

colourPref :: ReadM ColourPref
colourPref = OA.eitherReader f
  where
    f :: String -> Either String ColourPref
    f "always" = Right Always
    f "never"  = Right Never
    f "auto"   = Right Auto
    f _        = Left "the value must be \"always\", \"never\", or \"auto\""

defaultColourPref :: Bool -> ColourPref
defaultColourPref noColor
  | noColor   = Never
  | otherwise = Auto

path :: ReadM OsPath
path = OA.eitherReader f
  where
    f :: String -> Either String OsPath
    f = first show . OP.encodeWith utf8 utf16le

flagMap :: ReadM FlagMap
flagMap = OA.eitherReader f
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
  OA.hsubparser $ mconcat
  [ OA.command "init"
    ( OA.info initP
      (OA.progDesc "Create a new pkgsrc package")
    )
  , OA.command "update"
    ( OA.info updateP
      -- FIXME: We will most likely need to change this description because
      -- not all Cabal packages reside in the HackageDB.
      (OA.progDesc "Update an existing pkgsrc package to the latest version")
    )
  ]
  where
    initP :: Parser Command
    initP = (Init .) . InitOptions
            <$> OA.switch
                ( OA.long "overwrite" <>
                  OA.short 'w' <>
                  OA.help "Overwrite existing files"
                )
            <*> OA.argument OA.str (OA.metavar "TARBALL-URL")

    updateP :: Parser Command
    updateP = pure $ Update UpdateOptions


-- https://no-color.org/
-- FIXME: Document this env var
lookupNoColor :: MonadIO m => m Bool
lookupNoColor =
  do nc <- liftIO $ lookupEnv "NO_COLOR"
     pure $ case nc of
              Nothing -> False
              Just "" -> False
              Just _  -> True

parseOptions :: IO Options
parseOptions =
  do noColor <- lookupNoColor
     OA.customExecParser prefs (spec noColor)
  where
    prefs :: ParserPrefs
    prefs = OA.prefs . mconcat
            $ [ OA.subparserInline
              , OA.helpLongEquals
              , OA.helpShowGlobals
              ]

    spec :: Bool -> ParserInfo Options
    spec noColor =
      OA.info (optionsP noColor <**> OA.helper <**> OA.simpleVersioner ver)
      ( OA.fullDesc
        <> OA.header (PI.name <> " - " <> PI.synopsis)
      )

    ver :: String
    ver = showVersion PI.version


data Context
  = Context
    { ctxOptions      :: !Options
    , ctxUseColour    :: !Bool
    , ctxCanonPkgPath :: !(Deferred CLI OsPath)
    , ctxProgDb       :: !(Deferred CLI ProgramDb)
    , ctxIPI          :: !(Deferred CLI InstalledPackageIndex)
    , ctxSrcDb        :: !(Deferred CLI (SrcDb CLI))
    }

initialCtx :: (MonadThrow m, MonadUnliftIO m) => Options -> m Context
initialCtx opts
  = do col   <- case optColour opts of
                  Always -> pure True
                  Never  -> pure False
                  Auto   -> liftIO $ hNowSupportsANSI stderr
       pPath <- defer mkCanonPkgPath
       progs <- defer mkProgDb
       ipi   <- defer readPkgDb
       sdb   <- defer mkSrcDb
       pure Context
         { ctxOptions      = opts
         , ctxUseColour    = col
         , ctxCanonPkgPath = pPath
         , ctxProgDb       = progs
         , ctxIPI          = ipi
         , ctxSrcDb        = sdb
         }

mkCanonPkgPath :: CLI OsPath
mkCanonPkgPath =
  do dir  <- (liftIO . canonicalizePath) . optPkgPath =<< options
     -- Does it look like a package directory?
     p    <- liftIO $ doesFileExist (dir </> [osp|../../mk/bsd.pkg.mk|])
     unless p $
       do dir' <- T.pack <$> OP.decodeUtf dir
          fatal ( PP.dquotes (PP.annotate (PP.color PP.Cyan) (PP.pretty dir')) <+>
                  "doesn't look like a pkgsrc package directory"
                )
     pure dir

mkProgDb :: CLI ProgramDb
mkProgDb =
  do debug "Configuring program database..."
     ghcCmd      <- OP.decodeUtf . optGHCCmd =<< options
     (_, _, db0) <- liftIO $
                    GHC.configure silent (Just ghcCmd) Nothing C.defaultProgramDb
     db1         <- liftIO $
                    foldM (flip $ C.configureProgram silent) db0 bundledProgs
     debug $ "Obtained a database:\n" <> (PP.pretty . T.pack . ppShow $ db1)
     pure db1
  where
    -- We hate to hard-code these, but there seems to be no ways to avoid
    -- it.
    bundledProgs :: [C.Program]
    bundledProgs = [ C.haddockProgram
                   , C.hsc2hsProgram
                   ]

readPkgDb :: CLI InstalledPackageIndex
readPkgDb =
  do debug "Reading installed package index..."
     progs <- progDb
     liftIO $ GHC.getPackageDBContents silent C.GlobalPackageDB progs

mkSrcDb :: CLI (SrcDb CLI)
mkSrcDb =
  do make <- optMakeCmd <$> options
     root <- OP.takeDirectory . OP.takeDirectory <$> canonPkgPath
     createSrcDb make root


newtype CommandError = CommandError { message :: Doc AnsiStyle }
  deriving stock    Show
  deriving anyclass Exception

newtype CLI a = CLI { unCLI :: ReaderT Context (ResourceT IO) a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadCatch
                   , MonadFix
                   , MonadIO
                   , MonadMask
                   , MonadResource
                   , MonadThrow
                   , MonadUnliftIO
                   , PrimMonad
                   )

instance MonadFail CLI where
  fail :: String -> CLI a
  fail = fatal . PP.pretty . T.pack

instance Monoid a => Monoid (CLI a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (CLI a) where
  (<>) = liftA2 (<>)

runCLI :: CLI a -> IO a
runCLI m =
  do opts <- parseOptions
     ctx  <- initialCtx opts
     runResourceT (runReaderT (unCLI m) ctx)
       `catch`
       \(e :: CommandError) ->
         do useColour <- pure . ctxUseColour $ ctx
            print' useColour $ msgDoc e
            exitWith (ExitFailure 1)
  where
    msgDoc :: CommandError -> Doc AnsiStyle
    msgDoc e =
      progName <>
      PP.colon <+>
      PP.annotate (baseStyle <> PP.bold) "ERROR" <>
      PP.colon <+>
      PP.annotate baseStyle (message e) <>
      PP.hardline

    baseStyle :: AnsiStyle
    baseStyle = PP.colorDull PP.Red

options :: CLI Options
options = CLI $ asks ctxOptions

command :: CLI Command
command = optCommand <$> options

origPkgPath :: CLI OsPath
origPkgPath = optPkgPath <$> options

canonPkgPath :: CLI OsPath
canonPkgPath = CLI (asks ctxCanonPkgPath) >>= force

-- |@-d devel/foo@ => @devel@
category :: CLI Text
category = toText . OP.takeFileName . OP.takeDirectory =<< canonPkgPath
  where
    toText :: MonadThrow m => OsPath -> m Text
    toText = (T.pack <$>) . OP.decodeUtf

maintainer :: CLI (Maybe Text)
maintainer =
  -- FIXME: Document these env vars
  do m0 <- liftIO $ lookupEnv "PKGMAINTAINER"
     m1 <- liftIO $ lookupEnv "REPLYTO"
     pure $ T.pack <$> (m0 <|> m1)

makeCmd :: CLI OsPath
makeCmd = optMakeCmd <$> options

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

print :: Doc AnsiStyle -> CLI ()
print = (CLI (asks ctxUseColour) >>=) . flip print'

print' :: MonadIO m => Bool -> Doc AnsiStyle -> m ()
print' useColour doc =
  let doc'
        | useColour = doc
        | otherwise = PP.unAnnotate doc
  in
    liftIO $ PP.hPutDoc stderr doc'

progName :: Doc AnsiStyle
progName =
  PP.annotate PP.bold . PP.pretty . T.pack $ PI.name

-- |Print a debugging message to 'stderr'. The message should not end with
-- a linebreak.
debug :: Doc AnsiStyle -> CLI ()
debug msg =
  do d <- optDebug <$> options
     when d $
       print (progName <>
              PP.colon <+>
              PP.annotate (baseStyle <> PP.bold) "DEBUG" <>
              PP.colon <+>
              PP.annotate baseStyle msg <>
              PP.hardline)
  where
    baseStyle :: AnsiStyle
    baseStyle = PP.colorDull PP.Green

-- |Print an informational message to 'stderr'. The message should not end
-- with a linebreak.
info :: Doc AnsiStyle -> CLI ()
info msg =
  print (progName <>
         PP.colon <+>
         msg <>
         PP.hardline)

-- |Print a warning message to 'stderr'. The message should not end with
-- a linebreak.
warn :: Doc AnsiStyle -> CLI ()
warn msg =
  print (progName <>
         PP.colon <+>
         PP.annotate (baseStyle <> PP.bold) "WARNING" <>
         PP.colon <+>
         PP.annotate baseStyle msg <>
         PP.hardline)
  where
    baseStyle :: AnsiStyle
    baseStyle = PP.colorDull PP.Yellow

-- |Print an error message to 'stderr' and abort the process. The message
-- should not end with a linebreak.
fatal :: MonadThrow m => Doc AnsiStyle -> m a
fatal = throw . CommandError

-- |Run @make(1)@ with the working directory set to 'canonPkgPath'. The
-- child process is spawned with stdin closed, inheriting stderr, and
-- anything written to its stdout is redirected to stderr.
runMake :: [Text] -> CLI ()
runMake args =
  do make <- OP.decodeUtf =<< makeCmd
     dir  <- OP.decodeUtf =<< canonPkgPath
     let conf = PT.setStdin PT.nullStream
              . PT.setStdout (PT.useHandleOpen stderr)
              . PT.setWorkingDir dir
              $ PT.proc make (T.unpack <$> args)
     debug $ "Running " <> PP.pretty (show make) <+> PP.hsep (PP.pretty . show <$> args)
     PT.runProcess_ conf
