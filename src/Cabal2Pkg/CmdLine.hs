{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-} -- for deriving Exception
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , pkgBase
  , pkgCategory
  , pkgPath
  , origPkgDir
  , canonPkgDir
  , maintainer
  , makeCmd
  , pkgFlags
  , showPkgFlags
  , progDb
  , ghcVersion
  , hackageURI
  , installedPkgs
  , srcDb

    -- * Modifying the context
  , withPkgFlagsHidden
  , withPkgFlagsModified

    -- * Message output
  , debug
  , info
  , warn
  , fatal

    -- * Running @make(1)@
  , runMake
  ) where

import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.Static (makeQ)
import Control.Applicative ((<|>), (<**>), many, optional)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Exception.Safe
  ( Exception(..), Handler(..), MonadMask, SomeException, catches, throw )
import Control.Monad (foldM, unless, when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, local)
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
import GHC.Paths.PosixPath qualified as Paths
import Network.URI (URI, parseAbsoluteURI, parseURIReference)
import Network.URI.Static (uri)
import Options.Applicative (Parser, ParserInfo, ParserPrefs, ReadM)
import Options.Applicative qualified as OA
import Lens.Micro ((^.), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import PackageInfo_cabal2pkg qualified as PI
import Prelude hiding (print)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.Console.ANSI (hNowSupportsANSI)
import System.Directory.PosixPath (doesFileExist, canonicalizePath)
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitFailure)
import System.IO (stderr)
import System.OsPath.Posix ((</>), PosixPath, pstr)
import System.OsPath.Posix qualified as OP
import System.Process.Typed qualified as PT
import Text.Show.Pretty (ppShow)


data Command
  = Init   !InitOptions
  | Update !UpdateOptions
  deriving (Show)

data InitOptions
  = InitOptions
    { optOverwrite  :: !Bool
    , optPackageURI :: !URI
    }
  deriving (Show)

data UpdateOptions
  = UpdateOptions
    { optForce      :: !Bool
    , optPackageURI :: !(Maybe URI)
    }
  deriving (Show)

data ColourPref = Always | Never | Auto

instance Show ColourPref where
  show Always = "always"
  show Never  = "never"
  show Auto   = "auto"

type FlagMap = Map FlagName Bool

data Options
  = Options
    { _optCommand  :: !Command
      -- ^'Nothing' denotes @auto@
    , _optColour   :: !ColourPref
    , _optDebug    :: !Bool
    , _optPkgDir   :: !PosixPath
    , _optPkgFlags :: !FlagMap
    , _optGHCCmd   :: !PosixPath
    , _optHackage  :: !URI
    , _optMakeCmd  :: !PosixPath
    }
  deriving (Show)
makeLenses ''Options

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

newtype CommandError = CommandError { message :: Doc AnsiStyle }
  deriving stock    Show
  deriving anyclass Exception

-- |Print an error message to 'stderr' and abort the process. The message
-- should not end with a linebreak.
fatal :: MonadThrow m => Doc AnsiStyle -> m a
fatal = throw . CommandError

instance MonadFail CLI where
  fail :: String -> CLI a
  fail = fatal . PP.pretty . T.pack

instance Monoid a => Monoid (CLI a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (CLI a) where
  (<>) = liftA2 (<>)

data Context
  = Context
    { _ctxOptions     :: !Options
    , _ctxUseColour   :: !Bool
      -- ^Display flags defined in .cabal files.
    , _ctxShowFlags   :: !Bool
    , _ctxCanonPkgDir :: !(Deferred CLI PosixPath)
    , _ctxProgDb      :: !(Deferred CLI ProgramDb)
    , _ctxIPI         :: !(Deferred CLI InstalledPackageIndex)
    , _ctxSrcDb       :: !(Deferred CLI (SrcDb CLI))
    }
makeLenses ''Context

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
        OA.completeWith ["never", "always", "auto"] <>
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
      ( OA.long "pkgdir" <>
        OA.short 'p' <>
        OA.help "The path to the pkgsrc package to work with" <>
        OA.action "directory" <>
        OA.showDefault <>
        OA.value [pstr|.|] <>
        OA.metavar "DIR"
      )
  <*> ( M.unions <$> many
        ( OA.option flagMap
          ( OA.long "flag" <>
            OA.short 'f' <>
            OA.help "Cabal package flags to apply, such as \"+foo -bar\"" <>
            OA.metavar "FLAG"
          )
        )
      )
  <*> OA.option path
      ( OA.long "ghc" <>
        OA.help "The path to the GHC executable" <>
        OA.action "file" <>
        OA.showDefault <>
        OA.value Paths.ghc <>
        OA.metavar "FILE"
      )
  <*> OA.option absoluteURI
      ( OA.long "hackage" <>
        OA.help "The URI of an instance of Hackage repository to use" <>
        OA.showDefault <>
        OA.value [uri|https://hackage.haskell.org/|] <>
        OA.metavar "URI"
      )
  <*> OA.option path
      ( OA.long "make" <>
        OA.help "The path to the BSD make(1) command" <>
        OA.action "file" <>
        OA.showDefault <>
        OA.value $$makeQ <>
        OA.metavar "FILE"
      )

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

path :: ReadM PosixPath
path = OA.eitherReader f
  where
    f :: String -> Either String PosixPath
    f = first show . OP.encodeUtf

flagMap :: ReadM FlagMap
flagMap = OA.eitherReader f
  where
    f :: String -> Either String FlagMap
    f = second fa2Map . explicitEitherParsec C.legacyParsecFlagAssignment

    fa2Map :: C.FlagAssignment -> FlagMap
    fa2Map = M.fromList . C.unFlagAssignment

absoluteURI :: ReadM URI
absoluteURI = OA.maybeReader parseAbsoluteURI

uriReference :: ReadM URI
uriReference = OA.maybeReader parseURIReference

commandP :: Parser Command
commandP =
  OA.hsubparser $ mconcat
  [ OA.command "init"
    ( OA.info initP
      (OA.progDesc "Create a new pkgsrc package")
    )
  , OA.command "update"
    ( OA.info updateP
      (OA.progDesc "Update an existing pkgsrc package to a newer version")
    )
  ]
  where
    initP :: Parser Command
    initP =
      (Init .) . InitOptions
      <$> OA.switch
          ( OA.long "overwrite" <>
            OA.short 'w' <>
            OA.help "Overwrite existing files"
          )
      <*> OA.argument uriReference
          ( OA.help ( "http, https, or file URI to a package tarball." <>
                      " Or just a package name such as \"foo\" if the" <>
                      " package is from the Hackage repository. In the" <>
                      " latter case a version number can also be specified" <>
                      " like \"foo-0.1.2\""
                    ) <>
            OA.metavar "PACKAGE-URI"
          )

    updateP :: Parser Command
    updateP =
      (Update .) . UpdateOptions
      <$> OA.switch
          ( OA.long "force" <>
            OA.short 'f' <>
            OA.help ( "Update the package forcefully even if the requested" <>
                      " version is not a preferred one" )
          )
      <*> optional
          ( OA.argument uriReference
            ( OA.help ( "http, https, or file URI to an updated package" <>
                        " tarball. Or just a version number like" <>
                        " \"0.1.2\" if the package is from the Hackage" <>
                        " repository. Omit this if you want to update it" <>
                        " to the latest version" ) <>
              OA.metavar "PACKAGE-URI"
            )
          )

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
                -- We don't know if using OA.helpShowGlobals is a good idea
                -- or not.
              ]

    spec :: Bool -> ParserInfo Options
    spec noColor =
      OA.info (optionsP noColor <**> OA.helper <**> OA.simpleVersioner ver)
      ( OA.fullDesc
        <> OA.header (PI.name <> " - " <> PI.synopsis)
      )

    ver :: String
    ver = showVersion PI.version


initialCtx :: (MonadThrow m, MonadUnliftIO m) => Options -> m Context
initialCtx opts
  = do col   <- case opts ^. optColour of
                  Always -> pure True
                  Never  -> pure False
                  Auto   -> liftIO $ hNowSupportsANSI stderr
       pDir  <- defer mkCanonPkgDir
       progs <- defer mkProgDb
       ipi   <- defer readPkgDb
       sdb   <- defer mkSrcDb
       pure Context
         { _ctxOptions     = opts
         , _ctxUseColour   = col
         , _ctxShowFlags   = True
         , _ctxCanonPkgDir = pDir
         , _ctxProgDb      = progs
         , _ctxIPI         = ipi
         , _ctxSrcDb       = sdb
         }

mkCanonPkgDir :: CLI PosixPath
mkCanonPkgDir =
  do dir  <- (liftIO . canonicalizePath) . (^. optPkgDir) =<< options
     -- Does it look like a package directory?
     p    <- liftIO $ doesFileExist (dir </> [pstr|../../mk/bsd.pkg.mk|])
     unless p $
       fatal ( prettyAnsi dir <+>
               "doesn't look like a pkgsrc package directory" )
     pure dir

mkProgDb :: CLI ProgramDb
mkProgDb =
  do debug "Configuring program database..."
     ghcCmd      <- OP.decodeUtf . (^. optGHCCmd) =<< options
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
  do make <- (^. optMakeCmd) <$> options
     root <- OP.takeDirectory . OP.takeDirectory <$> canonPkgDir
     createSrcDb make root

-- |Run the 'CLI' monad in 'IO'. When a synchronous exception except for
-- 'ExitCode' is thrown, the function catches it, prints it to 'stderr',
-- and then invokes 'exitFailure'.
runCLI :: CLI a -> IO a
runCLI m =
  do opts <- parseOptions
     ctx  <- initialCtx opts
     runResourceT (runReaderT (unCLI m) ctx)
       `catches`
       [ Handler $ \(e :: ExitCode     ) -> throw e
       , Handler $ \(e :: CommandError ) -> die ctx $ message e
       , Handler $ \(e :: SomeException) -> die ctx $ PP.viaShow e
       ]
  where
    die :: Context -> Doc AnsiStyle -> IO a
    die ctx e =
      do print' (ctx ^. ctxUseColour) (msgDoc e)
         exitFailure

    msgDoc :: Doc AnsiStyle -> Doc AnsiStyle
    msgDoc e =
      progName <>
      PP.colon <+>
      PP.annotate (baseStyle <> PP.bold) "ERROR" <>
      PP.colon <+>
      PP.annotate baseStyle e <>
      PP.hardline

    baseStyle :: AnsiStyle
    baseStyle = PP.colorDull PP.Red

options :: CLI Options
options = CLI $ asks (^. ctxOptions)

command :: CLI Command
command = (^. optCommand) <$> options

origPkgDir :: CLI PosixPath
origPkgDir = (^. optPkgDir) <$> options

canonPkgDir :: CLI PosixPath
canonPkgDir = CLI (asks (^. ctxCanonPkgDir)) >>= force

-- |@-d /.../devel/foo@ => @devel/foo@
pkgPath :: CLI PosixPath
pkgPath =
  do cat  <- pkgCategory
     base <- pkgBase
     pure $ cat </> base

-- |@-d /.../devel/foo@ => @foo@
pkgBase :: CLI PosixPath
pkgBase = OP.takeFileName <$> canonPkgDir

-- |@-d /.../devel/foo@ => @devel@
pkgCategory :: CLI PosixPath
pkgCategory = OP.takeFileName . OP.takeDirectory <$> canonPkgDir

maintainer :: CLI (Maybe Text)
maintainer =
  -- FIXME: Document these env vars
  do m0 <- liftIO $ lookupEnv "PKGMAINTAINER"
     m1 <- liftIO $ lookupEnv "REPLYTO"
     pure $ T.pack <$> (m0 <|> m1)

makeCmd :: CLI PosixPath
makeCmd = (^. optMakeCmd) <$> options

pkgFlags :: CLI FlagMap
pkgFlags = (^. optPkgFlags) <$> options

showPkgFlags :: CLI Bool
showPkgFlags = CLI $ asks (^. ctxShowFlags)

progDb :: CLI ProgramDb
progDb = CLI (asks (^. ctxProgDb)) >>= force

ghcVersion :: CLI Version
ghcVersion
  = do progs <- progDb
       let ghc  = fromJust $ C.lookupKnownProgram "ghc" progs
           ghc' = fromJust $ C.lookupProgram ghc progs
           ver  = fromJust $ C.programVersion ghc'
       pure ver

hackageURI :: CLI URI
hackageURI = (^. optHackage) <$> options

installedPkgs :: CLI InstalledPackageIndex
installedPkgs = CLI (asks (^. ctxIPI)) >>= force

srcDb :: CLI (SrcDb CLI)
srcDb = CLI (asks (^. ctxSrcDb)) >>= force

print :: Doc AnsiStyle -> CLI ()
print = (CLI (asks (^. ctxUseColour)) >>=) . flip print'

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

withPkgFlagsHidden :: CLI a -> CLI a
withPkgFlagsHidden = CLI . local g . unCLI
  where
    g :: Context -> Context
    g = ctxShowFlags .~ False

withPkgFlagsModified :: (FlagMap -> FlagMap) -> CLI a -> CLI a
withPkgFlagsModified f = CLI . local g . unCLI
  where
    g :: Context -> Context
    g = ctxOptions . optPkgFlags %~ f

-- |Print a debugging message to 'stderr'. The message should not end with
-- a linebreak.
debug :: Doc AnsiStyle -> CLI ()
debug msg =
  do d <- (^. optDebug) <$> options
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

-- |Run @make(1)@ with the working directory set to 'canonPkgDir'. The
-- child process is spawned with stdin closed, inheriting stderr, and
-- anything written to its stdout is redirected to stderr.
runMake :: [Text] -> CLI ()
runMake args =
  do make <- OP.decodeUtf =<< makeCmd
     dir  <- OP.decodeUtf =<< canonPkgDir
     let conf = PT.setStdin PT.nullStream
              . PT.setStdout (PT.useHandleOpen stderr)
              . PT.setWorkingDir dir
              $ PT.proc make (T.unpack <$> args)
     debug $ "Running " <> PP.pretty (show make) <+> PP.hsep (PP.pretty . show <$> args)
     PT.runProcess_ conf
