{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, InitOptions(..), debug, fatal, info, pkgPath)
import Cabal2Pkg.Extractor
  ( PackageMeta(distBase), hasLibraries, hasExecutables, hasForeignLibs, summariseCabal )
import Cabal2Pkg.Generator.Buildlink3 (genBuildlink3)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Control.Exception.Safe (catch, throw)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import PackageInfo_cabal2pkg qualified as PI
import Prelude hiding (exp)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.IO (Handle, hClose)
import System.IO.Error (isAlreadyExistsError)
import System.OsPath ((</>), OsPath)
import System.OsPath qualified as OP
import System.OsString.Internal.Types (OsString(..))
import System.OsString.Posix (PosixString)
import System.Posix.IO.PosixString
  ( OpenFileFlags(..), OpenMode(..), defaultFileFlags, fdToHandle, openFd )
import Text.Show.Pretty (ppShow)

run :: HasCallStack => InitOptions -> CLI ()
run (InitOptions {..})
  = do info $ "Reading" <+> PP.dquotes (PP.pretty optTarballURL) <> "..."

       cabal <- readCabal optTarballURL
       debug $ "Found a package:\n" <> PP.pretty (ppShow cabal)

       meta <- summariseCabal cabal
       debug $ "Summarised package metadata:\n" <> PP.pretty (ppShow meta)

       dir <- pkgPath
       validatePkgPath meta dir

       --mkOut <- open' (dir </> "Makefile")

       let mk = genMakefile meta
       debug $ "Generated Makefile:\n" <> PP.pretty (TL.strip mk)

       let bl3 = genBuildlink3 meta
       debug $ "Generated buildlink3.mk:\n" <> PP.pretty (TL.strip bl3)
  where
    open' :: OsPath -> CLI Handle
    open' path
      = openFileForWrite' path
        `catch`
        \(e :: IOError) ->
          if isAlreadyExistsError e
          then do path' <- PP.pretty <$> OP.decodeUtf path
                  fatal ( "The file" <+>
                          PP.dquotes (PP.annotate (PP.color PP.Cyan) path') <+>
                          "already exists. If you want to overwrite it," <+>
                          "re-run" <+>
                          PP.dquotes (PP.annotate (PP.color PP.Cyan) command) <+>
                          "with -w"
                        )
          else throw e

    command :: Doc ann
    command = PP.pretty PI.name <+> "init"

    openFileForWrite' :: MonadResource m => OsPath -> m Handle
    openFileForWrite'
      | optOverwrite = openFileForWrite
      | otherwise    = openFreshFile

validatePkgPath :: MonadThrow m => PackageMeta -> OsPath -> m ()
validatePkgPath meta path
  = do actual <- T.pack <$> (OP.decodeUtf . OP.takeFileName) path
       let expWoPfx      = CI.foldCase . distBase $ meta
           expWPfx       = "hs-" <> expWoPfx
           expAndAct     :: Text -> Doc AnsiStyle
           expAndAct exp = "The package should be named" <+> pprName exp <+>
                           "but not" <+> pprName actual
           expAndAct'    = "The package should be named either" <+> pprName expWPfx <+>
                           "or" <+> pprName expWoPfx <+> "(depending on whether" <+>
                           "the fact it's implemented in Haskell is significant" <+>
                           "to users or the rest of the pkgsrc tree)" <+>
                           "but not" <+> pprName actual
           pfxNeeded     = "The prefix" <+> pprName "hs-" <+>
                           "indicates this is a Haskell library" <+>
                           "that can only be used in the Haskell world."
           pfxUnneeded   = "The prefix" <+> pprName "hs-" <+>
                           "indicates a Haskell library that can only be used in the" <+>
                           "Haskell world. However, this package only installs" <+>
                           "executables (or foreign libraries)" <+>
                           "that happen to be implemented in Haskell, which is of no" <+>
                           "significance to its users or the rest of the pkgsrc tree."
           renameDir     = "Please rename the directory and retry the command."
           pprName       :: Text -> Doc AnsiStyle
           pprName name  = PP.dquotes $
                           PP.annotate (PP.color PP.Cyan) (PP.pretty name)

       case shouldHaveHsPrefix meta of
         Just True ->
           let expected = expWPfx
           in
             if actual == expected then
               -- This is a good name
               pure ()

             else if actual == expWoPfx then
               -- Almost correct but without hs-
               fatal ( expAndAct expected <+> "to avoid name clashing." <+>
                       pfxNeeded <+> renameDir )

             else if CI.mk actual == CI.mk expected then
               -- Almost correct but wrongly cased
               fatal ( expAndAct expected <> ". We have a norm that" <+>
                       "new packages should have entirely lower-case names." <+>
                       renameDir )

             else if CI.mk actual == CI.mk expWoPfx then
               -- No prefix, wrongly cased
               fatal ( expAndAct expected <> "." <+> pfxNeeded <+>
                       "We also have a norm that new packages should have" <+>
                       "entirely lower-case names." <+> renameDir )
             else
               -- Totally incorrect
               fatal (expAndAct expected <> "." <+> renameDir)

         Just False ->
           let expected = expWoPfx
           in
             if actual == expected then
               -- This is a good name
               pure ()

             else if actual == expWPfx then
               -- Almost correct but superfluous hs-
               fatal ( expAndAct expected <> "." <+> pfxUnneeded <+>
                       renameDir )

             else if CI.mk actual == CI.mk expected then
               -- Almost correct but wrongly cased
               fatal ( expAndAct expected <> ". We have a norm that" <+>
                       "new packages should have entirely lower-case names." <+>
                       renameDir )

             else if CI.mk actual == CI.mk expWoPfx then
               -- Superfluous prefix, wrongly cased
               fatal (expAndAct expected <> "." <+> pfxUnneeded <+>
                      "We also have a norm that new packages should have" <+>
                      "entirely lower-case names." <+> renameDir)

             else
               -- Totally incorrect
               fatal (expAndAct expected <> "." <+> renameDir)

         Nothing ->
           if actual == expWPfx || actual == expWoPfx then
             -- This is a good name
             pure ()

           else if CI.mk actual == CI.mk expWPfx || CI.mk actual == CI.mk expWoPfx then
             -- Almost correct but wrongly cased
             fatal (expAndAct' <> ". We have a norm that" <+>
                    "new packages should have entirely lower-case names." <+>
                    renameDir)

           else
             -- Totally incorrect
             fatal (expAndAct' <> "." <+> renameDir)

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

--
-- If the package only provides Haskell libraries but no executables or
-- foreign libraries,
--   * It should have buildlink3.mk.
--   * PKGNAME should have a prefix "hs-".
--
-- If it only provides executables but nothing else,
--   * It shouldn't have buildlink3.mk.
--   * PKGNAME shouldn't have a prefix "hs-".
--
-- If it only provides foreign libraries but nothing else,
--   * It should have buildlink3.mk
--   * PKGNAME shouldn't have a prefix "hs-".
--
-- In any other cases,
--   * No rules as to whether to have buildlink3.mk. We generate one
--     anyway, and let the user decide if they want to keep it.
--   * No rules as to whether to have a prefix "hs-".
--
shouldHaveBuildlink3 :: PackageMeta -> Maybe Bool
shouldHaveBuildlink3 meta
  | hasLibraries meta && not (hasExecutables meta) && not (hasForeignLibs meta) = Just True
  | not (hasLibraries meta) && hasExecutables meta && not (hasForeignLibs meta) = Just False
  | not (hasLibraries meta) && not (hasExecutables meta) && hasForeignLibs meta = Just True
  | otherwise = Nothing

shouldHaveHsPrefix :: PackageMeta -> Maybe Bool
shouldHaveHsPrefix meta
  | hasLibraries meta && not (hasExecutables meta) && not (hasForeignLibs meta) = Just True
  | not (hasLibraries meta) && hasExecutables meta && not (hasForeignLibs meta) = Just False
  | not (hasLibraries meta) && not (hasExecutables meta) && hasForeignLibs meta = Just False
  | otherwise = Nothing
