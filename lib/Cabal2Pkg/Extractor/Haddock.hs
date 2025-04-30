{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Haddock
  ( extractHaddock
  ) where

import Data.Foldable (fold, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (stimes)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Language.Haddock.AST (parseHaddock)
import Language.Haddock.AST.Types
import Network.URI (uriToString)
import Prelude hiding (mod)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP

--import Debug.Trace (trace)
--import Text.Show.Pretty (ppShow)

-- |Consider a 'Text' to be Haddock marked-up and convert the resulting AST
-- to a @'Doc' 'AnsiStyle'@. Rendering the resulting 'Doc' yields a
-- plain-text form of the marked-up text with ANSI style annotations.
extractHaddock :: HasCallStack => Text -> Doc AnsiStyle
extractHaddock = pprHaddock
               . parseHaddock

pprHaddock :: Haddock -> Doc AnsiStyle
pprHaddock (Haddock blocks) = --trace (ppShow blocks) $
                              go (filter p blocks)
  where
    p :: Block -> Bool
    p (BTable _) = False
    p (BSince _) = False
    p _          = True

    -- Block separator (\n\n) should appear only between two consecutive
    -- blocks.
    go :: [Block] -> Doc AnsiStyle
    go []     = mempty
    go [b]    = pprBlock b
    go (b:bs) = pprBlock b <> PP.hardline <> PP.hardline <> go bs

pprBlock :: Block -> Doc AnsiStyle
pprBlock (BExamples  es) = pprExamples es
pprBlock (BTable     _ ) = error "impossible"
pprBlock (BSince     _ ) = error "impossible"
pprBlock (BUnordList ul) = pprUnordList ul
pprBlock (BOrdList   ol) = pprOrdList ol
pprBlock (BDefList   dl) = pprDefList dl
pprBlock (BCodeBlock cb) = pprCodeBlock cb
pprBlock (BProperty  p ) = pprProperty p
pprBlock (BHeading   hd) = pprHeading hd
pprBlock (BMath (BlockMath m)) = PP.pretty m
pprBlock (BParagraph is) = pprInlines is

pprExamples :: NonEmpty Example -> Doc AnsiStyle
pprExamples = go . toList
  where
    go :: [Example] -> Doc AnsiStyle
    go []     = mempty
    go [e]    = pprExample e
    go (e:es) = pprExample e <> PP.hardline <> PP.hardline <> go es

    pprExample :: Example -> Doc AnsiStyle
    pprExample (Example expr rs) =
      PP.vsep [ ">>>" <+> PP.pretty expr
              , PP.vsep (PP.pretty <$> rs)
              ]

pprUnordList :: UnordList -> Doc AnsiStyle
pprUnordList (UnordList items) = PP.vsep (go <$> items)
  where
    go :: Haddock -> Doc AnsiStyle
    go = (PP.pretty '*' <+>) . PP.hang 0 . pprHaddock

pprOrdList :: OrdList -> Doc AnsiStyle
pprOrdList (OrdList items) = PP.vsep (go <$> items)
  where
    go :: (Int, Haddock) -> Doc AnsiStyle
    go (idx, item) =
      PP.pretty idx <> PP.dot <+> PP.hang 0 (pprHaddock item)

pprDefList :: DefList -> Doc AnsiStyle
pprDefList (DefList items) = PP.vsep (go <$> items)
  where
    go :: (Inlines, Haddock) -> Doc AnsiStyle
    go (term, desc) =
      PP.vsep [ PP.pretty '*' <+> PP.hang 0 (pprInlines term <> PP.colon)
              , PP.indent 4 (pprHaddock desc)
              ]

pprCodeBlock :: CodeBlock -> Doc AnsiStyle
pprCodeBlock (CodeBlock is) = pprInlines is

pprProperty :: Property -> Doc AnsiStyle
pprProperty (Property txt) =
  PP.annotate PP.underlined ("prop>" <+> PP.pretty txt)

pprHeading :: Heading -> Doc AnsiStyle
pprHeading (Heading {..}) =
  PP.annotate PP.bold (hd <+> pprInlines title)
  where
    hd :: Doc AnsiStyle
    hd = stimes level (PP.pretty '#')

pprInlines :: Inlines -> Doc AnsiStyle
pprInlines (Inlines is) = fold (pprInline <$> is)

pprInline :: Inline -> Doc AnsiStyle
pprInline (IMonospaced (Monospaced   is)) = PP.annotate PP.underlined $ pprInlines is
pprInline (IEmphasis   (Emphasis     is)) = PP.annotate PP.italicized $ pprInlines is
pprInline (IBold       (Bold         is)) = PP.annotate PP.bold       $ pprInlines is
pprInline (IAnchor     _                ) = mempty
pprInline (IIdentifier (Identifier _ i )) = PP.annotate (PP.colorDull PP.Green) $ PP.pretty i
pprInline (IHyperlink  hl               ) = pprHyperlink hl
pprInline (IModLink    ml               ) = pprModLink ml
pprInline (IPicture    _                ) = mempty
pprInline (IMath       (InlineMath   m )) = PP.pretty m
pprInline (IWord       (Word         w )) = PP.pretty w
pprInline (ISpace      _                ) = PP.softline

pprHyperlink :: Hyperlink -> Doc AnsiStyle
pprHyperlink (Hyperlink uri Nothing) =
  PP.annotate (PP.colorDull PP.Cyan) (PP.pretty (uriToString id uri ""))
pprHyperlink (Hyperlink uri (Just label)) =
  PP.fillSep [ pprInlines label
             , PP.parens ( PP.annotate (PP.colorDull PP.Cyan)
                           . PP.pretty
                           $ uriToString id uri ""
                         )
             ]

pprModLink :: ModLink -> Doc AnsiStyle
pprModLink (ModLink mod Nothing ) = PP.annotate (PP.colorDull PP.Green) (PP.pretty mod)
pprModLink (ModLink _   (Just l)) = PP.annotate (PP.colorDull PP.Green) (pprInlines l )
