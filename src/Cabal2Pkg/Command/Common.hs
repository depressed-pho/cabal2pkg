{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Command.Common
  ( command
  , option
  , fetchMeta
  , shouldHaveBuildlink3
  , shouldHaveHsPrefix
  ) where

import Cabal2Pkg.CmdLine (CLI, debug, info)
import Cabal2Pkg.RawMeta (RawMeta(rmGPD), readRawMeta)
import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.Site (PackageURI, renderPackageURI)
import Cabal2Pkg.Extractor
  ( PackageMeta(changeLog), summariseCabal, hasLibraries, hasExecutables, hasForeignLibs )
import GHC.Stack (HasCallStack)
import PackageInfo_cabal2pkg qualified as PI
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import Text.Show.Pretty (ppShow)

command :: Doc AnsiStyle -> Doc AnsiStyle
command cmd =
  let cmd' = PP.pretty PI.name <+> cmd
  in
    PP.dquotes (PP.annotate (PP.colorDull PP.Green) cmd')

option :: Doc AnsiStyle -> Doc AnsiStyle
option = PP.annotate (PP.colorDull PP.Green)

-- |Fetch a package metadata from a URI. If the package URI starts with
-- hackageURI or it has no scheme, then it means this package came from
-- Hackage. In that case we omit MASTER_SITES and HOMEPAGE because
-- mk/haskell.mk takes care of them.
fetchMeta :: HasCallStack => PackageURI -> CLI PackageMeta
fetchMeta uri =
  do uri' <- renderPackageURI uri
     info $ PP.hsep [ "Fetching"
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
