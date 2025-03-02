{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Init
  ( run
  ) where

import Cabal2Pkg.CmdLine
  ( CLI, InitOptions(..), debug, fatal, fillColumn, info, warn, canonPkgDir
  , origPkgDir, makeCmd, pkgBase, runMake, wantCommitMsg )
import Cabal2Pkg.Command.Common
  ( command, option, fetchMeta, shouldHaveBuildlink3, shouldHaveHsPrefix
  , warnOutdated )
import Cabal2Pkg.Extractor (PackageMeta(distBase))
import Cabal2Pkg.Generator.Buildlink3 (genBuildlink3)
import Cabal2Pkg.Generator.CommitMsg (genImportMsg)
import Cabal2Pkg.Generator.Description (genDESCR)
import Cabal2Pkg.Generator.Makefile (genMakefile)
import Cabal2Pkg.Pretty (Quoted(..), prettyAnsi)
import Cabal2Pkg.Site (isFromLocalFS, parsePackageURI)
import Control.Exception.Safe (catch, throw)
import Control.Monad (when)
import Data.CaseInsensitive qualified as CI
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Distribution.Pretty (prettyShow)
import GHC.Stack (HasCallStack)
import Prelude hiding (exp, writeFile)
import Prettyprinter qualified as PP
import System.Directory.PosixPath (createDirectoryIfMissing)
import System.File.PosixPath.Alt (writeFile, writeFreshFile)
import System.IO.Error (isAlreadyExistsError)
import System.OsPath.Posix ((</>), PosixPath, pstr)
import System.OsPath.Posix qualified as OP

run :: HasCallStack => InitOptions -> CLI ()
run (InitOptions {..}) =
  do pkgURI <- parsePackageURI Nothing optPackageURI
     when (isFromLocalFS pkgURI) . warn $
       PP.hsep [ "You are initialising a package with a tarball on the local filesystem."
               , command "init"
               , "is assuming that the package has no upstreams, i.e. it will have no"
               , prettyAnsi (Quoted "MASTER_SITES")
               , "defined, but this is usually not the case. If not please specify a"
               , "remote file instead."
               ]
     meta <- fetchMeta pkgURI
     warnOutdated meta

     canonDir <- canonPkgDir
     validatePkgPath meta
     createDirectoryIfMissing True canonDir

     -- These files are generated from the package description. We don't
     -- overwrite existing files unless -w is given.
     width <- fillColumn
     let descr = genDESCR width meta
     debug $ "Generated DESCR:\n" <> PP.pretty (TL.strip descr)
     writeFile' [pstr|DESCR|] descr

     let mk = genMakefile meta
     debug $ "Generated Makefile:\n" <> PP.pretty (TL.strip mk)
     writeFile' [pstr|Makefile|] mk

     when (shouldHaveBuildlink3 meta /= Just False) $
       do let bl3 = genBuildlink3 meta
          debug $ "Generated buildlink3.mk:\n" <> PP.pretty (TL.strip bl3)
          writeFile' [pstr|buildlink3.mk|] bl3

     wantCMsg <- wantCommitMsg
     when wantCMsg $
       do let cMsg = genImportMsg width meta
          debug $ "Generated COMMIT_MSG:\n" <> PP.pretty (TL.strip cMsg)
          writeFile' [pstr|COMMIT_MSG|] cMsg

     -- PLIST cannot be directly generated from the package
     -- description. If it already exists we leave them unchanged.
     createFile' [pstr|PLIST|] =<< initialPLIST

     -- Generate distinfo, but the only way to do it is to run make(1).
     info "Generating distinfo..."
     runMake ["distinfo"]
  where
    writeFile' :: PosixPath -> TL.Text -> CLI ()
    writeFile' name txt =
      do let f | optOverwrite = writeFile
               | otherwise    = writeFreshFile
         cfp <- (</> name) <$> canonPkgDir
         ofp <- (</> name) <$> origPkgDir
         f cfp (TL.encodeUtf8 txt) `catch` \(e :: IOError) ->
           if isAlreadyExistsError e then
             fatal $ PP.hsep [ "The file"
                             , prettyAnsi ofp
                             , "already exists. If you want to overwrite it, re-run"
                             , command "init"
                             , "with"
                             , option "-w"
                             ]
           else
             throw e
         info $ "Wrote " <> prettyAnsi ofp

    createFile' :: PosixPath -> TL.Text -> CLI ()
    createFile' name txt =
      do cfp  <- (</> name) <$> canonPkgDir
         ofp  <- (</> name) <$> origPkgDir
         ofp' <- PP.pretty <$> OP.decodeUtf ofp
         ( do writeFreshFile cfp (TL.encodeUtf8 txt)
              info $ "Wrote " <> ofp'
           ) `catch` \(e :: IOError) ->
           if isAlreadyExistsError e then
             pure ()
           else
             throw e

initialPLIST :: CLI TL.Text
initialPLIST =
  do make <- TL.pack <$> (OP.decodeUtf . OP.takeFileName =<< makeCmd)
     pure . (<> "\n") . TL.intercalate "\n" $
       [ "@comment $NetBSD$"
       , "@comment TODO: To fill this file with the file listing:"
       , "@comment TODO: 1. Run \"" <> make <> " package\""
       , "@comment TODO: 1. Run \"" <> make <> " print-PLIST\""
       ]

validatePkgPath :: PackageMeta -> CLI ()
validatePkgPath meta
  = do actual   <- pkgBase
       expWoPfx <- OP.encodeUtf . CI.foldCase . prettyShow . distBase $ meta
       let expWPfx       = [pstr|hs-|] <> expWoPfx
           expAndAct exp = PP.hsep [ "The package should be named"
                                   , prettyAnsi exp
                                   , "but not"
                                   , prettyAnsi actual
                                   ]
           expAndAct'    = PP.hsep [ "The package should be named either"
                                   , prettyAnsi expWPfx
                                   , "or"
                                   , prettyAnsi expWoPfx
                                   , "(depending on whether the fact it's"
                                   , "implemented in Haskell is significant"
                                   , "to users or the rest of the pkgsrc tree)"
                                   , "but not"
                                   , prettyAnsi actual
                                   ]
           pfxNeeded     = PP.hsep [ "The prefix"
                                   , prettyAnsi [pstr|hs-|]
                                   , "indicates this is a Haskell library that"
                                   , "can only be used in the Haskell world."
                                   ]
           pfxUnneeded   = PP.hsep [ "The prefix"
                                   , prettyAnsi [pstr|hs-|]
                                   , "indicates a Haskell library that can only be used in the"
                                   , "Haskell world. However, this package only installs"
                                   , "executables (or foreign libraries) that happen to be"
                                   , "implemented in Haskell, which is of no significance"
                                   , "to its users or the rest of the pkgsrc tree."
                                   ]
           renameDir     = "Please rename the directory and retry the command."

       case shouldHaveHsPrefix meta of
         Just True ->
           let expected = expWPfx
           in
             if actual == expected then
               -- This is a good name
               pure ()

             else if actual == expWoPfx then
               -- Almost correct but without hs-
               fatal $ PP.hsep [ expAndAct expected
                               , "to avoid name clashing."
                               , pfxNeeded
                               , renameDir
                               ]

             else if CI.mk actual == CI.mk expected then
               -- Almost correct but wrongly cased
               fatal $ PP.hsep [ expAndAct expected <> ". We have a norm that"
                               , "new packages should have entirely lower-case names."
                               , renameDir
                               ]

             else if CI.mk actual == CI.mk expWoPfx then
               -- No prefix, wrongly cased
               fatal $ PP.hsep [ expAndAct expected <> "."
                               , pfxNeeded
                               , "We also have a norm that new packages should have"
                               , "entirely lower-case names."
                               , renameDir
                               ]

             else
               -- Totally incorrect
               fatal $ PP.hsep [ expAndAct expected <> "."
                               , renameDir
                               ]
         Just False ->
           let expected = expWoPfx
           in
             if actual == expected then
               -- This is a good name
               pure ()

             else if actual == expWPfx then
               -- Almost correct but superfluous hs-
               fatal $ PP.hsep [ expAndAct expected <> "."
                               , pfxUnneeded
                               , renameDir
                               ]

             else if CI.mk actual == CI.mk expected then
               -- Almost correct but wrongly cased
               fatal $ PP.hsep [ expAndAct expected <> ". We have a norm that"
                               , "new packages should have entirely lower-case names."
                               , renameDir
                               ]

             else if CI.mk actual == CI.mk expWoPfx then
               -- Superfluous prefix, wrongly cased
               fatal $ PP.hsep [ expAndAct expected <> "."
                               , pfxUnneeded
                               , "We also have a norm that new packages should have"
                               , "entirely lower-case names."
                               , renameDir
                               ]

             else
               -- Totally incorrect
               fatal $ PP.hsep [ expAndAct expected <> "."
                               , renameDir
                               ]

         Nothing ->
           if actual == expWPfx || actual == expWoPfx then
             -- This is a good name
             pure ()

           else if CI.mk actual == CI.mk expWPfx || CI.mk actual == CI.mk expWoPfx then
             -- Almost correct but wrongly cased
             fatal $ PP.hsep [ expAndAct' <> ". We have a norm that"
                             , "new packages should have entirely lower-case names."
                             , renameDir
                             ]

           else
             -- Totally incorrect
             fatal $ PP.hsep [ expAndAct' <> "."
                             , renameDir
                             ]
