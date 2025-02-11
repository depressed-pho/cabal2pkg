{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Generator.CommitMsg
  ( genImportMsg
  , genUpdateMsg
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Generator.Description (genDESCR)
import Data.Algorithm.Diff (PolyDiff(..), getGroupedDiff)
import Data.List (find)
import Data.Text.Lazy qualified as TL
import Distribution.Pretty (prettyShow)
import Numeric.Natural (Natural)


genImportMsg :: Natural -> PackageMeta -> TL.Text
genImportMsg width meta@(PackageMeta {..}) =
  mconcat [ TL.fromStrict pkgPath
          , ": import "
          , TL.pack $ prettyShow distBase
          , "-"
          , TL.pack $ prettyShow distVersion
          , "\n\n"
          , genDESCR width meta
          ]

genUpdateMsg :: PackageMeta -> PackageMeta -> TL.Text
genUpdateMsg oldMeta newMeta =
  mconcat [ TL.fromStrict (pkgPath newMeta)
          , ": update to "
          , TL.pack $ prettyShow (distBase newMeta)
          , "-"
          , TL.pack $ prettyShow (distVersion newMeta)
          , "\n\n"
          , changes
          ]
  where
    noRelNotes :: TL.Text
    noRelNotes =
      "No release notes have been provided by the upstream."

    changes :: TL.Text
    changes =
      case (changeLog oldMeta, changeLog newMeta) of
        (_, Nothing) ->
          noRelNotes

        (Nothing, Just new) ->
          new

        (Just old, Just new) ->
          -- Take a diff between these two, and choose the first hunk of
          -- addition as the update. This might not be accurate but should
          -- be a good guess.
          let diff = getGroupedDiff (TL.lines old) (TL.lines new)
          in
            case find isSecond diff of
              Just (Second ls) -> (<> "\n") . TL.strip . TL.unlines $ ls
              Just _           -> error "logically impossible"
              Nothing          -> noRelNotes

    isSecond :: PolyDiff a b -> Bool
    isSecond Second {} = True
    isSecond _         = False
