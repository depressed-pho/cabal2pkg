{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Cabal2Pkg.Extractor.Haddock
  ( extractHaddock
  ) where

import Data.Bifunctor (bimap, second)
import Data.Char (isSpace)
import Data.List qualified as L
import Data.MonoTraversable (Element, MonoPointed(..))
import Data.Semigroup (stimes)
import Data.Text (Text)
import Data.Text qualified as T
import Documentation.Haddock.Types
  ( DocH(..), DocMarkupH(..), Example(..), Header(..), Hyperlink(..)
  , MetaDoc(_doc), ModLink(..) )
import Documentation.Haddock.Markup qualified as Haddock
import Documentation.Haddock.Parser qualified as Haddock
import GHC.Stack (HasCallStack)
import Prelude hiding (lines, words)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import Text.Show.Pretty (ppShow)

--import Debug.Trace (traceWith)

data DocTree ann =
    Point  !(Doc ann)
    -- |'Bool' indicates whether there should be a space before or after
    -- lines.
  | Lines  !Bool ![Doc ann] !Bool
  | Para   !(Doc ann)
  deriving Show

-- THINKME: This is a HUGE mess. I don't like this code at all. But what
-- else can we do? The root cause of this messiness is that the Haddock AST
-- isn't convenient for us at all. We should roll up our own Haddock
-- parser and throw this abomination away.
instance Semigroup (DocTree ann) where
  (Point a) <> (Point b) = Point (a <> b)

  (Point a) <> (Lines pfxSpc bs sfxSpc) =
    case L.uncons bs of
      Nothing        -> Lines False [a] sfxSpc
      Just (b', bs')
        | pfxSpc     -> Lines False (PP.fillSep [a, b'] : bs') sfxSpc
        | otherwise  -> Lines False (a <> b'            : bs') sfxSpc

  (Point a) <> (Para b) = Para (a <> b)

  (Lines pfxSpc as sfxSpc) <> (Point b) =
    case L.unsnoc as of
      Nothing        -> Point b
      Just (as', a')
        | sfxSpc     -> Lines pfxSpc (as' <> [PP.fillSep [a', b]]) False
        | otherwise  -> Lines pfxSpc (as' <> [a' <> b]           ) False

  (Lines pfxSpcA as sfxSpcA) <> (Lines pfxSpcB bs sfxSpcB) = go as bs
    where
      -- [Doc ann] represents lines. Concatenation of two lists xs and ys
      -- therefore means 'mappend'-ing the last line of xs and the first
      -- line of ys.
      go :: [Doc ann] -> [Doc ann] -> DocTree ann
      go []     ys     = Lines (pfxSpcA || sfxSpcA || pfxSpcB) ys sfxSpcB
      go [x]    []     = Lines pfxSpcA [x] (sfxSpcA || pfxSpcB || sfxSpcB)
      go [x]    (y:ys)
        | sfxSpcA || pfxSpcB = Lines pfxSpcA (PP.fillSep [x, y] : ys) sfxSpcB
        | otherwise          = Lines pfxSpcA (x <> y            : ys) sfxSpcB
      go (x:xs) ys     = case go xs ys of
                           Lines _ zs sfxSpcC -> Lines pfxSpcA (x:zs) sfxSpcC
                           _                  -> error "impossible case"

  (Lines _ _ _) <> (Para _) = error "absurd AST"

  (Para a) <> (Point b) = Para (a <> b)

  (Para _) <> (Lines _ _ _) = error "absurd AST"

  (Para a) <> (Para b) = Para (a <> PP.hardline <> PP.hardline <> b)

instance Monoid (DocTree ann) where
  mempty = Point mempty

type instance Element (DocTree ann) = Doc ann
instance MonoPointed (DocTree ann) where
  opoint = Point

-- |Consider a 'Text' to be Haddock marked-up and convert the resulting AST
-- to a @'Doc' 'AnsiStyle'@. Rendering the resulting 'Doc' yields a
-- plain-text form of the marked-up text with ANSI style annotations.
extractHaddock :: HasCallStack => Text -> Doc AnsiStyle
extractHaddock = unwrap
               . Haddock.markup (plainTextMarkup (error "unsupported") PP.pretty)
               -- . traceWith (ppShow :: DocH () String -> String)
               . fixup
               . Haddock.toRegular
               . _doc
               . Haddock.parseParas Nothing
               . T.unpack

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

unwrap :: HasCallStack => DocTree ann -> Doc ann
unwrap (Point doc) = doc
unwrap (Para  doc) = doc
unwrap dt          = error ("absurd AST: " <> ppShow dt)

-- |A set of instructions for marking up Haddock documentation as plain
-- text.
plainTextMarkup :: HasCallStack
                => (mod -> Doc AnsiStyle)
                -> (id -> Doc AnsiStyle)
                -> DocMarkupH mod id (DocTree AnsiStyle)
plainTextMarkup fromMod fromId =
  Markup { markupEmpty               = mempty
         , markupString              = text . T.pack
         , markupParagraph           = paragraph
         , markupAppend              = (<>)
         , markupIdentifier          = opoint . PP.annotate (PP.colorDull PP.Green) . fromId
         , markupIdentifierUnchecked = opoint . PP.annotate (PP.colorDull PP.Green) . fromMod
         , markupModule              = opoint . PP.annotate (PP.colorDull PP.Green) . modLink
         , markupWarning             = opoint . warning
         , markupEmphasis            = opoint . PP.annotate PP.italicized . fillSepLines
         , markupBold                = opoint . PP.annotate PP.bold       . fillSepLines
         , markupMonospaced          = opoint . PP.annotate PP.underlined . fillSepLines
         , markupUnorderedList       = unordList
         , markupOrderedList         = ordList
         , markupDefList             = defList
         , markupCodeBlock           = codeBlock
         , markupHyperlink           = opoint . hyperlink
         , markupAName               = pure mempty
         , markupPic                 = pure mempty
         , markupMathInline          = text . T.pack
         , markupMathDisplay         = text . T.pack
         , markupProperty            = opoint . PP.pretty . T.pack
         , markupExample             = examples
         , markupHeader              = opoint . header
           -- THINKME: Not implemented, but do we really want to put tables
           -- in our DESCR files or flag information? They're extremely
           -- hard to render because cells have colspan and rowspan.
         , markupTable               = const mempty
         }
  where
    foldLines :: HasCallStack => ([Doc ann] -> Doc ann) -> DocTree ann -> Doc ann
    foldLines _ (Point   doc    ) = doc
    foldLines f (Lines _ lines _) = f lines
    foldLines _ (Para    dt     ) = error ("absurd AST: " <> ppShow dt)

    fillSepLines :: HasCallStack => DocTree ann -> Doc ann
    fillSepLines = foldLines PP.fillSep

    text :: Text -> DocTree ann
    text = go . T.split (== '\n')
      where
        go :: [Text] -> DocTree ann
        go ls =
          case L.uncons ls of
            Nothing      -> mempty
            Just (a, as) ->
              let pfx = T.takeWhile isSpace a
                  ls' = T.drop (T.length pfx) a : as
              in
                go' (not $ T.null pfx) ls'

        go' :: Bool -> [Text] -> DocTree ann
        go' pfxSpc ls =
          case L.unsnoc ls of
            Nothing      -> Lines pfxSpc mempty False
            Just (bs, b) ->
              let sfx = T.takeWhileEnd isSpace b
                  ls' = bs <> [T.dropEnd (T.length sfx) b]
              in
                Lines pfxSpc (wrap <$> ls') (not $ T.null sfx)

        wrap :: Text -> Doc ann
        wrap = PP.fillSep . (PP.pretty <$>) . T.words

    paragraph :: HasCallStack => DocTree ann -> DocTree ann
    paragraph = Para . fillSepLines

    codeBlock :: HasCallStack => DocTree ann -> DocTree ann
    codeBlock = Para . foldLines PP.vsep

    unordList :: HasCallStack => [DocTree ann] -> DocTree ann
    unordList = Para . PP.vsep . (go <$>)
      where
        go :: HasCallStack => DocTree ann -> Doc ann
        go = (PP.pretty '*' <+>) . PP.hang 0 . fillSepLines

    ordList :: HasCallStack => [(Int, DocTree ann)] -> DocTree ann
    ordList = Para . PP.vsep . (go <$>)
      where
        go :: HasCallStack => (Int, DocTree ann) -> Doc ann
        go (idx, item) =
          PP.pretty idx <> PP.dot <+> PP.hang 0 (fillSepLines item)

    defList :: HasCallStack => [(DocTree ann, DocTree ann)] -> DocTree ann
    defList = Para . PP.vsep . (go <$>)
      where
        go :: HasCallStack => (DocTree ann, DocTree ann) -> Doc ann
        go (term, desc) =
          PP.vsep [ PP.pretty '*' <+> PP.hang 0 (fillSepLines term <> PP.colon)
                  , PP.indent 4 (fillSepLines desc)
                  ]

    examples :: [Example] -> DocTree ann
    examples = Para . PP.vsep . (go <$>)
      where
        go :: Example -> Doc ann
        go ex =
          PP.vsep [ ">>>" <+> PP.pretty (T.pack $ exampleExpression ex)
                  , PP.vsep $ PP.pretty . T.pack <$> exampleResult ex
                  ]

    header :: Header (DocTree ann) -> Doc ann
    header hd =
      stimes (headerLevel hd) (PP.pretty '#') <+> fillSepLines (headerTitle hd)

    hyperlink :: Hyperlink (DocTree AnsiStyle) -> Doc AnsiStyle
    hyperlink hl =
      case hyperlinkLabel hl of
        Nothing -> PP.annotate urlStyle . PP.pretty . T.pack . hyperlinkUrl $ hl
        Just l  -> PP.fillSep [ fillSepLines l
                              , PP.parens ( PP.annotate urlStyle
                                          . PP.pretty
                                          . T.pack
                                          . hyperlinkUrl $ hl
                                          )
                              ]
      where
        urlStyle :: AnsiStyle
        urlStyle = PP.colorDull PP.Cyan

    modLink :: ModLink (DocTree ann) -> Doc ann
    modLink ml =
      case modLinkLabel ml of
        Nothing -> PP.pretty . T.pack $ modLinkName ml
        Just l  -> fillSepLines l

    warning :: DocTree AnsiStyle -> Doc AnsiStyle
    warning = (PP.annotate (baseStyle <> PP.bold) "Warning:" <+>)
            . PP.nest 2
            . PP.annotate baseStyle
            . fillSepLines
      where
        baseStyle :: AnsiStyle
        baseStyle = PP.colorDull PP.Yellow
