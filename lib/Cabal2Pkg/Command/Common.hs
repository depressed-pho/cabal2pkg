{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Command.Common
  ( command
  , command'
  , option

  , fetchMeta
  , shouldHaveBuildlink3
  , shouldHaveHsPrefix
  , warnDeps
  ) where

import Cabal2Pkg.CmdLine (CLI, debug, info, warn)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep(..))
import Cabal2Pkg.Extractor.Dependency.Library (LibDep(..))
import Cabal2Pkg.Extractor.Dependency.Version (cmpRange)
import Cabal2Pkg.Extractor
  ( PackageMeta(..), summariseCabal, hasLibraries, hasExecutables, hasForeignLibs )
import Data.Text qualified as T
import Distribution.Types.PackageId (PackageIdentifier(..))
import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.RawMeta (RawMeta(rmGPD), readRawMeta)
import Cabal2Pkg.Site (PackageURI, isFromLocalFS, renderPackageURI)
import Data.Generics.Aliases (GenericQ, mkQ, extQ)
import Data.Generics.Schemes (everything)
import GHC.Stack (HasCallStack)
import PackageInfo_cabal2pkg qualified as PI
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import System.OsPath.Posix qualified as OP
import Text.Show.Pretty (ppShow)

command :: Doc AnsiStyle -> Doc AnsiStyle
command cmd =
  let cmd' = PP.pretty PI.name <+> cmd
  in
    PP.dquotes (PP.annotate (PP.colorDull PP.Green) cmd')

command' :: Doc AnsiStyle
command' = PP.pretty PI.name

option :: Doc AnsiStyle -> Doc AnsiStyle
option = PP.annotate (PP.colorDull PP.Green)

-- |Fetch a package metadata from a URI. If the package URI starts with
-- hackageURI or it has no scheme, then it means this package came from
-- Hackage. In that case we omit MASTER_SITES and HOMEPAGE because
-- mk/haskell.mk takes care of them.
fetchMeta :: HasCallStack => PackageURI -> CLI PackageMeta
fetchMeta uri =
  do uri' <- renderPackageURI uri
     if isFromLocalFS uri
       then info $ "Analysing" <+> prettyAnsi uri' <> "..."
       else info $ PP.hsep [ "Fetching"
                           , prettyAnsi uri'
                           , "and analysing its package description..."
                           ]

     rawMeta <- readRawMeta uri
     debug $ "Found a package description:\n" <> PP.pretty (ppShow . rmGPD $ rawMeta)

     meta <- summariseCabal rawMeta
     debug $ "Summarised package metadata:\n" <> PP.pretty (ppShow . stripChangeLog $ meta)

     pure meta

-- Because no one would want to read lengthy ChangeLog even while
-- debugging.
stripChangeLog :: PackageMeta -> PackageMeta
stripChangeLog pm =
  case changeLog pm of
    Nothing -> pm
    Just _  -> pm { changeLog = Just "..." }

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

-- |Warn about dependencies that are too old to satisfy version
-- constraints.
warnDeps :: PackageMeta -> CLI ()
warnDeps pm = everything (>>) go pm
  where
    go :: GenericQ (CLI ())
    go = mkQ (pure ()) checkExeDep `extQ` checkLibDep

    pkgId :: PackageIdentifier
    pkgId = PackageIdentifier (distBase pm) (distVersion pm)

    checkExeDep :: ExeDep -> CLI ()
    checkExeDep (KnownBundledExe {..}) =
      case cmpRange version verRange of
        Just LT ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a compiler-bundled tool"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but its version is currently", prettyAnsi version <> "."
                         , "You first need to update the compiler to use this package."
                         ]
        Just _  -> pure ()
        Nothing ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a compiler-bundled tool"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but this version constraint is impossible to satisfy."
                         , command', "does not know what to do."
                         ]
    checkExeDep (KnownPkgsrcExe {..}) =
      case cmpRange version verRange of
        Just LT ->
          do path <- OP.encodeUtf . T.unpack $ pkgPath
             warn $ PP.hsep [ prettyAnsi pkgId, "requires a tool"
                            , prettyAnsi name, prettyAnsi verRange
                            , "but its version is currently", prettyAnsi version <> "."
                            , "You first need to update", prettyAnsi path
                            , "or it will fail to build."
                            ]
        Just _  -> pure ()
        Nothing ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a tool"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but this version constraint is impossible to satisfy."
                         , command', "does not know what to do."
                         ]
    checkExeDep (UnknownExe {..}) =
      warn $ PP.hsep [ prettyAnsi pkgId, "requires a tool"
                     , prettyAnsi name, prettyAnsi verRange
                     , "but there are no pkgsrc packages providing it."
                     , "You first need to package it to build"
                     , prettyAnsi pkgId <> "."
                     ]

    checkLibDep :: LibDep -> CLI ()
    checkLibDep (KnownBundledLib {..}) =
      case cmpRange version verRange of
        Just LT ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a compiler-bundled library"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but its version is currently", prettyAnsi version <> "."
                         , "You first need to update the compiler to use this package."
                         ]
        Just _  -> pure ()
        Nothing ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a compiler-bundled library"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but this version constraint is impossible to satisfy."
                         , command', "does not know what to do."
                         ]
    checkLibDep (KnownPkgsrcLib {..}) =
      case cmpRange version verRange of
        Just LT ->
          do path <- OP.encodeUtf . T.unpack $ pkgPath
             warn $ PP.hsep [ prettyAnsi pkgId, "requires a library"
                            , prettyAnsi name, prettyAnsi verRange
                            , "but its version is currently", prettyAnsi version <> "."
                            , "You first need to update", prettyAnsi path
                            , "or it will fail to build."
                            ]
        Just _  -> pure ()
        Nothing ->
          warn $ PP.hsep [ prettyAnsi pkgId, "requires a library"
                         , prettyAnsi name, prettyAnsi verRange
                         , "but this version constraint is impossible to satisfy."
                         , command', "does not know what to do."
                         ]
    checkLibDep (UnknownLib {..}) =
      warn $ PP.hsep [ prettyAnsi pkgId, "requires a library"
                     , prettyAnsi name, prettyAnsi verRange
                     , "but there are no pkgsrc packages providing it."
                     , "You first need to package it to build"
                     , prettyAnsi pkgId <> "."
                     ]
