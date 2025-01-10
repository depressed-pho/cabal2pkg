{-# LANGUAGE OverloadedStrings #-}
-- |A high-level interface to the @diff3@ package.
module Data.Text.Lazy.Merge
  ( -- * Performing 3-way merge
    MarkerStyle(..)
  , merge

    -- * Checking if a file contains conflict markers
  , hasMarkers

    -- * Low-level utility
  , linesWithLF
  ) where

import Data.Algorithm.Diff3 (Hunk(..), diff3)
import Data.Semigroup (stimes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB


data MarkerStyle =
    -- ^Use RCS-style conflict marker:
    --
    -- > <<<<<<< changed file A
    -- > lines from the changed file A
    -- > =======
    -- > lines from the changed file B
    -- > >>>>>>> changed file B
    RCS
    -- ^Use diff3-style conflict marker:
    --
    -- > <<<<<<< changed file A
    -- > lines from the changed file A
    -- > ||||||| merge base
    -- > lines from the merge base
    -- > =======
    -- > lines from the changed file B
    -- > >>>>>>> changed file B
  | Diff3
  deriving (Eq, Show)

-- |Perform a 3-way merge. Each input file is represented as a @(label,
-- contents)@ pair where @label@ is the label that appears in conflict
-- markers.
merge :: MarkerStyle
      -> (Text, TL.Text) -- ^Changed file A
      -> (Text, TL.Text) -- ^Merge base; the common ancestor of A and B
      -> (Text, TL.Text) -- ^Changed file B
      -> TL.Text         -- ^Merged file; possibly contains conflict markers
merge ms (labelA, fileA) (labelBase, fileBase) (labelB, fileB) =
  let linesA    = linesWithLF fileA
      linesBase = linesWithLF fileBase
      linesB    = linesWithLF fileB
      hunks     = diff3 linesA linesBase linesB
  in
    mconcat $ renderHunk <$> hunks
  where
    renderHunk :: Hunk TL.Text -> TL.Text
    renderHunk (LeftChange  ts) = mconcat ts
    renderHunk (RightChange ts) = mconcat ts
    renderHunk (Unchanged   ts) = mconcat ts
    renderHunk (Conflict as os bs)
      | as == bs  = mconcat as
                    -- A and B both changed Base in exactly the same
                    -- way. These aren't really conflicts in the usual
                    -- sense but 'diff3' reports these as so.
      | otherwise =
          TLB.toLazyText $
          mconcat [ marker '<' labelA
                  , mconcat $ TLB.fromLazyText <$> as
                  , case ms of
                      RCS   -> mempty
                      Diff3 -> mconcat [ marker '|' labelBase
                                       , mconcat $ TLB.fromLazyText <$> os
                                       ]
                  , marker '=' mempty
                  , mconcat $ TLB.fromLazyText <$> bs
                  , marker '>' labelB
                  ]

    marker :: Char -> Text -> TLB.Builder
    marker c label
      | T.null label = m <> TLB.singleton '\n'
      | otherwise    = mconcat [ m
                               , TLB.singleton ' '
                               , TLB.fromText label
                               , TLB.singleton '\n'
                               ]
      where
        m = stimes (7 :: Int) (TLB.singleton c)

-- |Check if a file contains conflict markers.
hasMarkers :: TL.Text -> Bool
hasMarkers = any ("<<<<<<< " `TL.isPrefixOf`) . TL.lines

-- |This is similar to 'TL.lines' but resulting lines contain newlines,
-- except that the last line may not have a newline. The inverse of this
-- function is 'mconcat'.
linesWithLF :: TL.Text -> [TL.Text]
linesWithLF = go mempty . TL.toChunks
  where
    go :: TL.Text -> [Text] -> [TL.Text]
    go _ []     = []
    go l (c:cs) =
      case T.findIndex (== '\n') c of
        Just i ->
          let (line0, rest) = T.splitAt (i + 1) c
              line          = l <> TL.fromChunks [line0]
          in
            line : go mempty (rest : cs)
        Nothing ->
          let leftover = l <> TL.fromChunks [c]
          in
            go leftover cs
