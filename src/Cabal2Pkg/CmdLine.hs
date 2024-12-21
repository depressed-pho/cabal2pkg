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
  , progDb
  , ghcVersion
  , hackageURI
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
import Control.Applicative ((<|>), (<**>), many, optional)
import Control.Concurrent.Deferred (Deferred, defer, force)
import Control.Exception.Safe
  ( Exception(..), Handler(..), MonadMask, SomeException, catches, throw )
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
import Network.URI (URI, parseAbsoluteURI, parseURIReference)
import Network.URI.Static (uri)
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
    , optPkgDir   :: !OsPath
    , optPkgFlags :: !FlagMap
    , optGHCCmd   :: !OsPath
    , optHackage  :: !URI
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
        OA.value [osp|.|] <>
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

absoluteURI :: ReadM URI
absoluteURI = OA.maybeReader parseAbsoluteURI

uriReference :: ReadM URI
uriReference = OA.maybeReader parseURIReference

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

newtype UpdateOptions
  = UpdateOptions
    { optPackageURI :: Maybe URI
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
      Update . UpdateOptions
      <$> optional
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


data Context
  = Context
    { ctxOptions     :: !Options
    , ctxUseColour   :: !Bool
    , ctxCanonPkgDir :: !(Deferred CLI OsPath)
    , ctxProgDb      :: !(Deferred CLI ProgramDb)
    , ctxIPI         :: !(Deferred CLI InstalledPackageIndex)
    , ctxSrcDb       :: !(Deferred CLI (SrcDb CLI))
    }

initialCtx :: (MonadThrow m, MonadUnliftIO m) => Options -> m Context
initialCtx opts
  = do col   <- case optColour opts of
                  Always -> pure True
                  Never  -> pure False
                  Auto   -> liftIO $ hNowSupportsANSI stderr
       pDir  <- defer mkCanonPkgDir
       progs <- defer mkProgDb
       ipi   <- defer readPkgDb
       sdb   <- defer mkSrcDb
       pure Context
         { ctxOptions     = opts
         , ctxUseColour   = col
         , ctxCanonPkgDir = pDir
         , ctxProgDb      = progs
         , ctxIPI         = ipi
         , ctxSrcDb       = sdb
         }

mkCanonPkgDir :: CLI OsPath
mkCanonPkgDir =
  do dir  <- (liftIO . canonicalizePath) . optPkgDir =<< options
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
     root <- OP.takeDirectory . OP.takeDirectory <$> canonPkgDir
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
       `catches`
       [ Handler $ \(e :: CommandError ) -> die ctx $ message e
       , Handler $ \(e :: SomeException) -> die ctx $ PP.viaShow e
       ]
  where
    die :: Context -> Doc AnsiStyle -> IO a
    die ctx e =
      do useColour <- pure . ctxUseColour $ ctx
         print' useColour $ msgDoc e
         exitWith (ExitFailure 1)

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
options = CLI $ asks ctxOptions

command :: CLI Command
command = optCommand <$> options

origPkgDir :: CLI OsPath
origPkgDir = optPkgDir <$> options

canonPkgDir :: CLI OsPath
canonPkgDir = CLI (asks ctxCanonPkgDir) >>= force

-- |@-d /.../devel/foo@ => @devel/foo@
pkgPath :: CLI Text
pkgPath =
  do dir  <- canonPkgDir
     name <- OP.decodeUtf . OP.takeFileName $ dir
     cat  <- OP.decodeUtf . OP.takeFileName . OP.takeDirectory $ dir
     pure . T.pack $ cat <> "/" <> name

-- |@-d /.../devel/foo@ => @foo@
pkgBase :: CLI Text
pkgBase = pathToText . OP.takeFileName =<< canonPkgDir

-- |@-d /.../devel/foo@ => @devel@
pkgCategory :: CLI Text
pkgCategory = pathToText . OP.takeFileName . OP.takeDirectory =<< canonPkgDir

pathToText :: MonadThrow m => OsPath -> m Text
pathToText = (T.pack <$>) . OP.decodeUtf

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

hackageURI :: CLI URI
hackageURI = optHackage <$> options

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
