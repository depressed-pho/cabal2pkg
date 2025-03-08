{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator.Description
  ( genDESCR
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Data.Bifunctor (bimap, second)
import Data.Char (isSpace)
import Data.Semigroup (stimes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Documentation.Haddock.Types
  ( DocH(..), DocMarkupH(..), Example(..), Header(..), Hyperlink(..)
  , MetaDoc(_doc), ModLink(..) )
import Documentation.Haddock.Markup qualified as Haddock
import Documentation.Haddock.Parser qualified as Haddock
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Prelude hiding (lines, words)
import Prettyprinter ((<+>), Doc, LayoutOptions(..), PageWidth(..))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text (renderLazy)

--import Debug.Trace (traceWith)
--import Text.Show.Pretty (ppShow)


genDESCR :: HasCallStack => Natural -> PackageMeta -> TL.Text
genDESCR width = (<> "\n")
                 . TL.strip
                 . renderLazy
                 . PP.layoutPretty opts
                 . mconcat
                 . Haddock.markup (plainTextMarkup undefined PP.pretty)
                 -- . traceWith (ppShow :: DocH () String -> String)
                 . fixup
                 . Haddock.toRegular
                 . _doc
                 . Haddock.parseParas Nothing
                 . T.unpack
                 . description
  where
    opts :: LayoutOptions
    opts = LayoutOptions
           { layoutPageWidth = AvailablePerLine (fromIntegral width) 1
           }

-- |Strip 'DocParagraph' in lists.
fixup :: DocH mod id -> DocH mod id
fixup (DocAppend       x y) = DocAppend        (fixup x) (fixup y)
fixup (DocUnorderedList xs) = DocUnorderedList (fixup' <$> xs)
fixup (DocOrderedList   xs) = DocOrderedList   (second fixup' <$> xs)
fixup (DocDefList       xs) = DocDefList       (bimap fixup' fixup' <$> xs)
fixup d = d

fixup' :: DocH mod id -> DocH mod id
fixup' (DocParagraph d) = d
fixup' d = d

-- |A set of instructions for marking up Haddock documentation as plain
-- text. The resulting list of @'Doc' ann@ should be simply 'mconcat'-ed
-- before rendering.
plainTextMarkup :: HasCallStack
                => (mod -> Doc ann)
                -> (id -> Doc ann)
                -> DocMarkupH mod id [Doc ann]
plainTextMarkup fromMod fromId =
  Markup { markupEmpty               = pure mempty
         , markupString              = text . T.pack
         , markupParagraph           = pure . paragraph
         , markupAppend              = append
         , markupIdentifier          = pure . fromId
         , markupIdentifierUnchecked = pure . fromMod
         , markupModule              = pure . modLink
         , markupWarning             = pure . warning
         , markupEmphasis            = id
         , markupBold                = id
         , markupMonospaced          = id
         , markupUnorderedList       = pure . unordList
         , markupOrderedList         = pure . ordList
         , markupDefList             = pure . defList
         , markupCodeBlock           = pure . codeBlock
         , markupHyperlink           = pure . hyperlink
         , markupAName               = pure mempty
         , markupPic                 = pure mempty
         , markupMathInline          = text . T.pack
         , markupMathDisplay         = text . T.pack
         , markupProperty            = pure . PP.pretty . T.pack
         , markupExample             = pure . examples
         , markupHeader              = pure . header
           -- THINKME: Not implemented, but do we really want to put tables
           -- in our DESCR files? They're extremely hard to render because
           -- cells have colspan and rowspan.
         , markupTable               = pure mempty
         }
  where
    -- [Doc ann] represents lines. Concatenation of two lists xs and ys
    -- therefore means 'mappend'-ing the last line of xs and the first line
    -- of ys.
    append :: [Doc ann] -> [Doc ann] -> [Doc ann]
    append []     ys     = ys
    append [x]    []     = [x]
    append [x]    (y:ys) = (x <> y) : ys
    append (x:xs) ys     = x : append xs ys

    text :: Text -> [Doc ann]
    text = (wrap <$>) . T.split (== '\n')
      where
        -- Lines may contain leading or trailing spaces, and we must
        -- preserve them. Also using T.split instead of T.lines for the
        -- same reason.
        wrap :: Text -> Doc ann
        wrap txt =
          let pfx   = T.takeWhile isSpace txt
              txt'  = T.drop (T.length pfx) txt
              sfx   = T.takeWhileEnd isSpace txt'
              txt'' = T.dropEnd (T.length sfx) txt'
              words = PP.fillSep . (PP.pretty <$>) . T.words $ txt''
          in
            PP.pretty pfx <> words <> PP.pretty sfx

    paragraph :: [Doc ann] -> Doc ann
    paragraph = (<> PP.line) . (<> PP.line) . PP.fillSep

    codeBlock :: [Doc ann] -> Doc ann
    codeBlock = (<> PP.line) . (<> PP.line) . PP.vsep

    unordList :: [[Doc ann]] -> Doc ann
    unordList = (<> PP.line) . (<> PP.line) . PP.vsep . (go <$>)
      where
        go :: [Doc ann] -> Doc ann
        go = (PP.pretty '*' <+>) . PP.hang 0 . PP.hsep

    ordList :: [(Int, [Doc ann])] -> Doc ann
    ordList = (<> PP.line) . (<> PP.line) . PP.vsep . (go <$>)
      where
        go :: (Int, [Doc ann]) -> Doc ann
        go (idx, item) =
          PP.pretty idx <> PP.dot <+> PP.hang 0 (PP.hsep item)

    defList :: [([Doc ann], [Doc ann])] -> Doc ann
    defList = (<> PP.line) . (<> PP.line) . PP.vsep . (go <$>)
      where
        go :: ([Doc ann], [Doc ann]) -> Doc ann
        go (term, desc) =
          PP.vsep [ PP.pretty '*' <+> PP.hang 0 (PP.hsep term <> PP.colon)
                  , PP.indent 4 (PP.hsep desc)
                  ]

    examples :: [Example] -> Doc ann
    examples = (<> PP.line) . (<> PP.line) . PP.vsep . (go <$>)
      where
        go :: Example -> Doc ann
        go ex =
          PP.vsep [ ">>>" <+> PP.pretty (T.pack $ exampleExpression ex)
                  , PP.vsep $ PP.pretty . T.pack <$> exampleResult ex
                  ]

    header :: Header [Doc ann] -> Doc ann
    header hd =
      stimes (headerLevel hd) (PP.pretty '#') <+> mconcat (headerTitle hd)

    hyperlink :: Hyperlink [Doc ann] -> Doc ann
    hyperlink hl =
      case hyperlinkLabel hl of
        Nothing -> PP.pretty . T.pack $ hyperlinkUrl hl
        Just l  -> PP.fillSep [ mconcat l
                              , PP.parens (PP.pretty . T.pack $ hyperlinkUrl hl)
                              ]

    modLink :: ModLink [Doc ann] -> Doc ann
    modLink ml =
      case modLinkLabel ml of
        Nothing -> PP.pretty . T.pack $ modLinkName ml
        Just l  -> mconcat l

    warning :: [Doc ann] -> Doc ann
    warning = ("Warning:" <+>) . PP.nest 2 . mconcat
