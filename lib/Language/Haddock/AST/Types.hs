module Language.Haddock.AST.Types
  ( Haddock(..)

  , Block(..)
  , Example(..)
  , Table(..)
  , TableRow(..)
  , TableCell(..)
  , Since(..)
  , UnordList(..)
  , OrdList(..)
  , DefList(..)
  , CodeBlock(..)
  , Property(..)
  , Heading(..)
  , BlockMath(..)

  , Inlines(..)

  , Inline(..)
  , Monospaced(..)
  , Emphasis(..)
  , Bold(..)
  , Anchor(..)
  , Identifier(..)
  , Namespace(..)
  , Hyperlink(..)
  , ModLink(..)
  , InlineMath(..)
  , Picture(..)
  , Word(..)
  , Space(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Version (Version)
import Network.URI (URI)
import Prelude hiding (Word)

-- We have our own Haddock AST and parser. We know we have haddock-library,
-- but its AST is way too inconvenient for us. The main problem with its
-- DocH is that everything is in the flat data type and there are
-- ridiculously many ways to construct absurd AST with it.

newtype Haddock = Haddock [Block]
  deriving (Eq, Monoid, Semigroup, Show)

data Block =
    BExamples  !(NonEmpty Example)
  | BTable     !Table
  | BSince     !Since
  | BUnordList !UnordList
  | BOrdList   !OrdList
  | BDefList   !DefList
  | BCodeBlock !CodeBlock
  | BProperty  !Property
  | BHeading   !Heading
  | BMath      !BlockMath
  | BParagraph !Inlines
  deriving (Eq, Show)

data Example =
  Example
  { expression :: !Text
  , results    :: ![Text]
  }
  deriving (Eq, Show)

data Table =
  Table
  { header :: ![TableRow]
  , body   :: ![TableRow]
  }
  deriving (Eq, Show)

newtype TableRow = TableRow [TableCell]
  deriving (Eq, Show)

data TableCell =
  TableCell
  { colSpan  :: !Int
  , rowSpan  :: !Int
  , contents :: !Haddock
  }
  deriving (Eq, Show)

newtype Since = Since Version
  deriving (Eq, Show)

newtype UnordList = UnordList [Haddock]
  deriving (Eq, Show)

newtype OrdList = OrdList [(Int, Haddock)]
  deriving (Eq, Show)

newtype DefList = DefList [(Inlines, Haddock)]
  deriving (Eq, Show)

newtype CodeBlock = CodeBlock Inlines
  deriving (Eq, Show)

newtype Property = Property Text
  deriving (Eq, Show)

data Heading =
  Heading
  { level :: !Int
  , title :: !Inlines
  }
  deriving (Eq, Show)

newtype Inlines = Inlines (NonEmpty Inline)
  deriving (Eq, Semigroup, Show)

data Inline =
    IMonospaced !Monospaced
  | IEmphasis   !Emphasis
  | IBold       !Bold
  | IAnchor     !Anchor
  | IIdentifier !Identifier
  | IHyperlink  !Hyperlink
  | IModLink    !ModLink
  | IPicture    !Picture
  | IMath       !InlineMath
  | IWord       !Word
  | ISpace      !Space
  deriving (Eq, Show)

newtype Monospaced = Monospaced Inlines
  deriving (Eq, Show)

newtype Emphasis = Emphasis Inlines
  deriving (Eq, Show)

newtype Bold = Bold Inlines
  deriving (Eq, Show)

newtype Anchor = Anchor Text
  deriving (Eq, Show)

data Identifier = Identifier !Namespace !Text
  deriving (Eq, Show)

data Namespace = Value | Type | None
  deriving (Eq, Show)

data Hyperlink =
  Hyperlink
  { hlTarget :: !URI
  , hlLabel  :: !(Maybe Inlines)
  }
  deriving (Eq, Show)

data ModLink =
  ModLink
  { mlTarget :: !Text
  , mlLabel  :: !(Maybe Inlines)
  }
  deriving (Eq, Show)

data Picture =
  Picture
  { picURI   :: !URI
  , picTitle :: !(Maybe Text)
  }
  deriving (Eq, Show)

newtype BlockMath = BlockMath Text
  deriving (Eq, Show)

newtype InlineMath = InlineMath Text
  deriving (Eq, Show)

-- |Words usually don't contain any whitespaces, except in code blocks.
newtype Word = Word Text
  deriving (Eq, Show)

-- |Consecutive whitespaces are collapsed to a single SPC (0x20).
data Space = Space
  deriving (Eq, Show)
