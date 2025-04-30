{-# LANGUAGE OverloadedStrings #-}
module Language.Haddock.AST.Parse
  ( parseHaddock
  ) where

import Control.Applicative ((<|>), empty)
import Control.Monad (MonadPlus, guard, mfilter, void, when)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as P
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (makeVersion)
import GHC.Stack (HasCallStack)
import Language.Haddock.AST.Types
import Network.URI (URI, parseURIReference)
import Prelude hiding (Word, lines)
import Text.Read.Lex (isSymbolChar)

class Parsable a where
  parse :: Parser a

emptyLine :: Parser ()
emptyLine = void $ horizSpaces *> P.char '\n'

emptyLines :: Parser ()
emptyLines = P.skipMany emptyLine

horizSpaces :: Parser Text
horizSpaces = P.takeWhile f
  where
    f :: Char -> Bool
    f ' '  = True
    f '\t' = True
    f _    = False

nonEmptyLine :: Parser Text
nonEmptyLine = mfilter (T.any (not . isSpace)) line

rejectLF :: Parser Text -> Parser Text
rejectLF = mfilter (T.all (/= '\n'))

line :: Parser Text
line = P.takeWhile (/= '\n') <* endOfLine

endOfLine :: Parser ()
endOfLine = (void $ P.char '\n') <|> P.endOfInput

-- |Take the indentation of the first non-empty line.
indent :: Parser Text
indent =
  do ind <- horizSpaces
     (P.char '\n' *> indent) <|> pure ind

-- |Like 'P.takeWhile' but unconditionally take escaped characters.
takeWhile_ :: (Char -> Bool) -> Parser Text
takeWhile_ p = P.scan False p'
  where
    p' :: Bool -> Char -> Maybe Bool
    p' escaped c
      | escaped   = Just False
      | not (p c) = Nothing
      | otherwise = Just (c == '\\')

-- |Like 'P.takeWhile1' but unconditionally take escaped characters.
takeWhile1_ :: (Char -> Bool) -> Parser Text
takeWhile1_ = mfilter (not . T.null) . takeWhile_

-- |Consume characters from the input up to and including the given
-- pattern. Return everything consumed except for the end pattern
-- itself. The resulting text must not be empty, or the parse will fail.
takeUntil1 :: Text -> Parser Text
takeUntil1 end = T.dropEnd (T.length end) <$> requireEnd (P.scan (False, end) p) >>= gotSome
  where
    p :: (Bool, Text) -> Char -> Maybe (Bool, Text)
    p st c
      | (True, _  )  <- st =
          -- The previous character was escaped. Restart matching.
          Just (False, end)

      | (_   , pat)  <- st
      , Nothing      <- T.uncons pat =
          -- No more remaining characters in the pattern.
          Nothing

      | (_   , pat)  <- st
      , Just (x, xs) <- T.uncons pat
      , x == c =
          -- Found a single character of the ending pattern.
          Just (False, xs)

      | otherwise =
          Just (c == '\\', end)

    requireEnd :: Parser Text -> Parser Text
    requireEnd = mfilter (T.isSuffixOf end)

    gotSome :: Text -> Parser Text
    gotSome xs
      | T.null xs = fail "empty match"
      | otherwise = pure xs

-- |Parse and construct a thing that optionally has a label.
mkLabeled :: Applicative f
          => (a -> Maybe b -> c)
          -> (Text -> f a) -- ^Main parser
          -> (Text -> f b) -- ^Label parser
          -> Text
          -> f c
mkLabeled mk parseA parseL txt =
  case T.break isSpace . removeEscapes . T.strip $ txt of
    (txtA, "" ) -> mk <$> parseA txtA <*> pure Nothing
    (txtA, lbl) -> mk <$> parseA txtA <*> (Just <$> parseL (T.stripStart lbl))

instance Parsable Haddock where
  parse = Haddock <$> (emptyLines *> P.many' (parse <* emptyLines))

instance Parsable Block where
  parse = P.choice [ BExamples . NE.fromList <$> P.many1' (emptyLines *> parse)
                   , BTable <$> parse
                   , do ind <- indent
                        P.choice [ BSince     <$> parse
                                 , BUnordList <$> unordList ind
                                 , BOrdList   <$> ordList   ind
                                   -- Markdown links have a higher
                                   -- precedence than definition lists.
                                 , BParagraph <$> paragraphThatStartsWithMarkdownLink
                                 , BDefList   <$> defList   ind
                                 , BCodeBlock <$> verbatimCodeBlock
                                 , BCodeBlock <$> parsedCodeBlock
                                 , BProperty  <$> parse
                                 , BHeading   <$> parse
                                 , BMath      <$> parse
                                 , BParagraph <$> paragraph
                                 ]
                   ]

instance Parsable Example where
  parse =
    do prefix <- horizSpaces <* ">>>"
       expr   <- T.strip <$> line
       rs     <- P.many' (result prefix)
       pure $ Example expr rs
    where
      result :: Text -> Parser Text
      result prefix = substBlank . tryStripPrefix <$> nonEmptyLine
        where
          tryStripPrefix :: Text -> Text
          tryStripPrefix txt =
            fromMaybe txt (T.stripPrefix prefix txt)

      substBlank :: Text -> Text
      substBlank txt
        | txt == "<BLANKLINE>" = ""
        | otherwise            = txt

instance Parsable Table where
  parse =
    do -- The first line determines the width of the table.
       line0 <- firstLine
       let width = T.length line0

       -- Parse all consecutive lines starting and ending with '+' or '|'
       -- of the same width.
       lines <- P.many1' (consecLine width)

       -- Now we gathered the table block, the next step is to split the
       -- block into rows.
       scanRows width (line0 : lines)
    where
      firstLine :: Parser Text
      firstLine =
        do void horizSpaces
           cs <- P.takeWhile (\c -> c == '-' || c == '+')

           -- Upper-left and upper-right corners are '+'.
           guard (T.length cs >= 2 &&
                  T.head cs == '+' &&
                  T.last cs == '+')

           -- The rest of the line is all spaces.
           emptyLine
           pure $ cs

      consecLine :: Int -> Parser Text
      consecLine width =
        do void horizSpaces
           cs <- P.scan width p

           -- Left and right edges are either '|' or '+'.
           guard (T.length cs == width)
           let l = T.head cs
               r = T.last cs
           guard ((l == '|' || l == '+') &&
                  (r == '|' || r == '+'))

           -- The rest of the line is all spaces.
           emptyLine
           pure $ cs
        where
          p :: Int -> Char -> Maybe Int
          p w c
            | w <= 0    = Nothing
            | c == '\n' = Nothing
            | otherwise = Just (w - 1)

data CellLoc =
  CellLoc
  { clLeft   :: !Int
  , clTop    :: !Int
  , clRight  :: !Int
  , clBottom :: !Int
  }
  deriving Show

-- The second step searches for a line consisting only of '+' and '=',
-- records its index, and changes '=' to '-'.
scanRows :: MonadFail m => Int -> [Text] -> m Table
scanRows width = go 0 []
  where
    go _ acc []     = scanCells width (reverse acc) Nothing
    go n acc (r:rs)
      | T.all (\c -> c == '+' || c == '=') r =
          scanCells width (reverse (r' : acc) <> rs) (Just n)
      | otherwise =
          go (n + 1) (r : acc) rs
      where
        r' :: Text
        r' = T.map (\c -> if c == '=' then '-' else c) r

-- The third step recognises cells in the table block, returning a list of
-- cells.
scanCells :: MonadFail m => Int -> [Text] -> Maybe Int -> m Table
scanCells width lines hdrIdx =
  do cellLocs <- go (S.singleton (0, 0))
     formTable lines hdrIdx cellLocs
  where
    height :: Int
    height = length lines

    go :: Monad m => Set (Int, Int) -> m [CellLoc]
    go queue =
      case S.minView queue of
        Nothing -> pure []
        Just ((x, y), queue')
          | x + 1 >= width || y + 1 >= height -> go queue'
          | otherwise ->
              case scanRight x y of
                Nothing       -> go queue'
                Just (x', y') ->
                  do let cl = CellLoc x y x' y'
                     (cl :) <$> go (queue' `S.union`
                                     S.fromList [(x', y), (x, y'), (x', y')])

    -- Scan right looking for '+', then scan down.
    scanRight :: Int -> Int -> Maybe (Int, Int)
    scanRight x y = go' (x + 1)
      where
        l :: Text
        l = lines !! y

        go' :: Int -> Maybe (Int, Int)
        go' x' | x' >= width         = fail "overflow: right"
               | '+' <- T.index l x' = scanDown x y x' <|> go' (x' + 1)
               | '-' <- T.index l x' = go' (x' + 1)
               | otherwise           = fail "not a border: right"

    -- Scan down looking for '+'.
    scanDown :: Int -> Int -> Int -> Maybe (Int, Int)
    scanDown x y x' = go' (y + 1)
      where
        go' :: Int -> Maybe (Int, Int)
        go' y' | y' >= height        = fail "overflow: down"
               | '+' <- T.index l x' = scanLeft x y x' y' <|> go' (y' + 1)
               | '|' <- T.index l x' = go' (y' + 1)
               | otherwise           = fail "not a border: down"
          where
            l :: Text
            l = lines !! y'

    -- Verify that all the characters x..x' y' are either '+' or '-'.
    scanLeft :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
    scanLeft x y x' y'
      | all p [x..x'] = scanUp x y x' y'
      | otherwise     = fail "not a border: left"
      where
        p :: Int -> Bool
        p x'' =
          let c = T.index l x''
          in c == '+' || c == '-'

        l :: Text
        l = lines !! y'

    -- Verify that all the characters x y..y' are either '+' or '|'.
    scanUp :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
    scanUp x y x' y'
      | all p [y..y'] = pure (x', y')
      | otherwise     = fail "not a border: top"
      where
        p :: Int -> Bool
        p y'' =
          let l = lines !! y''
              c = T.index l x
          in c == '+' || c == '|'

-- The fourth step forms a 'Table' structure.
formTable :: MonadFail m => [Text] -> Maybe Int -> [CellLoc] -> m Table
formTable lines hdrIdx cellLocs
  | Nothing <- hdrIdx = Table [] <$> rows
  | Just i  <- hdrIdx =
      case L.elemIndex i yTabStops of
        Nothing -> Table [] <$> rows
        Just i' -> uncurry Table . L.splitAt i' <$> rows
  where
    xTabStops :: [Int]
    xTabStops = sortNub . concatMap clXS $ cellLocs

    yTabStops :: [Int]
    yTabStops = sortNub . concatMap clYS $ cellLocs

    sortNub :: Ord a => [a] -> [a]
    sortNub = S.toList . S.fromList

    clXS :: CellLoc -> [Int]
    clXS (CellLoc l _ r _) = [l, r]

    clYS :: CellLoc -> [Int]
    clYS (CellLoc _ t _ b) = [t, b]

    init' :: [a] -> [a]
    init' xs
      | Just (xs', _) <- L.unsnoc xs = xs'
      | otherwise                    = []

    rows :: MonadFail m => m [TableRow]
    rows = mapM makeRow (init' yTabStops)

    makeRow :: MonadFail m => Int -> m TableRow
    makeRow y = TableRow . catMaybes <$> mapM (makeCell y) cellLocs

    makeCell :: MonadFail m => Int -> CellLoc -> m (Maybe TableCell)
    makeCell y (CellLoc l t r b)
      | y /= t    = pure Nothing
      | otherwise = Just . TableCell xts yts <$> extract (l + 1) (y + 1) (r - 1) (b - 1)
      where
        xts :: Int
        xts = length . L.takeWhile (< r) . L.dropWhile (< l) $ xTabStops

        yts :: Int
        yts = length . L.takeWhile (< b) . L.dropWhile (< y) $ yTabStops

    -- Extract cell contents given boundaries.
    extract :: MonadFail m => Int -> Int -> Int -> Int -> m Haddock
    extract l t r b =
      parseBlocks $
      T.intercalate "\n" [ T.strip . T.take (r - l + 1) . T.drop l $ (lines !! y)
                         | y <- [t..b]
                         ]

instance Parsable Since where
  parse =
    do _ <- "@since"
       _ <- horizSpaces
       v <- makeVersion <$> P.sepBy1' P.decimal (P.char '.')
       _ <- horizSpaces
       _ <- endOfLine
       pure . Since $ v

unordList :: Text -> Parser UnordList
unordList ind = UnordList <$> go
  where
    go :: Parser [Haddock]
    go = (P.char '*' <|> P.char '-') *> innerList ind go

ordList :: Text -> Parser OrdList
ordList ind = OrdList <$> go
  where
    go :: Parser [(Int, Haddock)]
    go = do idx <- paren <|> dot
            innerList' ind go idx

    dot :: Parser Int
    dot = P.decimal <* P.char '.'

    paren :: Parser Int
    paren = P.char '(' *> P.decimal <* P.char ')'

defList :: Text -> Parser DefList
defList ind = DefList <$> go
  where
    go :: Parser [(Inlines, Haddock)]
    go = do label       <- P.char '[' *>
                           (parseInlines True =<< takeWhile1_ (\c -> c /= ']' && c /= '\n'))
                           <* (P.char ']' <* P.option Nothing (Just <$> P.char ':'))
            l           <- line
            (ls, items) <- more ind go
            contents    <- Haddock . L.singleton . BParagraph <$>
                           (parseInlines True . T.strip . T.unlines $ l : ls)
            pure $ case items of
                     Left  h -> [(label, contents <> h)]
                     Right i ->  (label, contents) : i

-- Collect any further lines belonging to the list entry, and recursively
-- collect any further lists in the same paragraph.
innerList :: Text -> Parser [Haddock] -> Parser [Haddock]
innerList ind item =
  do l           <- line
     (ls, items) <- more ind item
     contents    <- Haddock . L.singleton . BParagraph <$>
                    (parseInlines True . T.strip . T.unlines $ l : ls)
     pure $ case items of
              Left  h -> [contents <> h]
              Right i -> contents : i

-- Like 'innerList' but takes a parsed index of the list item.
innerList' :: Text -> Parser [(Int, Haddock)] -> Int -> Parser [(Int, Haddock)]
innerList' ind item idx =
  do l           <- line
     (ls, items) <- more ind item
     contents    <- Haddock . L.singleton . BParagraph <$>
                    (parseInlines True . T.strip . T.unlines $ l : ls)
     pure $ case items of
              Left  h -> [(idx, contents <> h)]
              Right i ->  (idx, contents) : i

-- Main worker for 'innerList' and 'defList'. Uses 'Either' to be able to
-- tell in the respective functions whether we're dealing with the next
-- list or a nested paragraph.
more :: Monoid a => Text -> Parser a -> Parser ([Text], Either Haddock a)
more ind item =
  P.choice [ innerBlocks ind
           , moreListItems ind item
           , moreContents ind item
           , pure ([], Right mempty)
           ]

-- Used by 'innerList' and 'defList' to parse any nested blocks.
innerBlocks :: Text -> Parser ([Text], Either Haddock a)
innerBlocks ind = (,) [] . Left <$> (P.char '\n' *> indentedBlocks ind)

-- Attempt to fetch the next list if possible. Used by 'innerList' and
-- 'defList' to recursively grab lists that aren't separated by a whole
-- paragraph.
moreListItems :: forall a. Text -> Parser a -> Parser ([Text], Either Haddock a)
moreListItems ind item = (,) [] . Right <$> indentedItem
  where
    indentedItem :: Parser a
    indentedItem = P.string ind *> P.skipWhile isSpace *> item

-- Used by 'innerList' and 'defList' which simply takes a line of text and
-- attempts to parse more list contents with 'more'.
moreContents :: Monoid a => Text -> Parser a -> Parser ([Text], Either Haddock a)
moreContents ind item = first . (:) <$> nonEmptyLine <*> more ind item

-- Parse an indented block. The indentation has to be 4 spaces according to
-- the spec.
indentedBlocks :: Text -> Parser Haddock
indentedBlocks ind =
  (T.concat <$> dropFrontOfBlock indent') >>= parseBlocks
  where
    indent' :: Parser Text
    indent' = P.string (ind <> "    ")

-- Grab as many fully indented blocks as we can.
dropFrontOfBlock :: Parser Text -> Parser [Text]
dropFrontOfBlock ind =
  do cur  <- P.many1' (ind *> ((`T.snoc` '\n') <$> nonEmptyLine))
     rest <- P.choice [ horizSpaces  *> nextBlock -- We have more blocks to take.
                      , horizSpaces  *> lfList    -- End of the ride, remember the newline.
                      , P.endOfInput *> return [] -- Nothing more to take at all.
                      ]
     pure $ cur <> rest
  where
    nextBlock :: Parser [Text]
    nextBlock = (<>) <$> lfList <*> dropFrontOfBlock ind

    lfList :: Parser [Text]
    lfList = P.char '\n' *> return ["\n"]

verbatimCodeBlock :: Parser CodeBlock
verbatimCodeBlock = CodeBlock . Inlines . (:| []) . IWord . Word .
                    T.intercalate "\n" . stripSpace
                    <$> P.many1' go
  where
    go :: Parser Text
    go = horizSpaces *> P.char '>' *> line

    stripSpace :: [Text] -> [Text]
    stripSpace = fromMaybe <*> mapM strip'

    strip' :: Text -> Maybe Text
    strip' t =
      case T.uncons t of
        Nothing        -> Just ""
        Just (' ', t') -> Just t'
        _              -> Nothing -- This cancels stripping for all lines,
                                  -- not just this one.

parsedCodeBlock :: Parser CodeBlock
parsedCodeBlock =
  do txt <- P.char '@' *> horizSpaces *> P.char '\n' *> cbText <* P.char '@'
     is  <- parseInlines False (T.stripEnd . dropSpaces $ txt)
     pure $ CodeBlock is
  where
    dropSpaces :: Text -> Text
    dropSpaces txt =
      let ls = splitByLF txt
      in case L.unsnoc ls of
           Nothing     -> txt
           Just (_, l) ->
             case T.uncons l of
               Just (' ', _) ->
                 case mapM dropSpace ls of
                   Nothing  -> txt
                   Just ls' -> T.intercalate "\n" ls'
               _ -> txt

    -- Like L.lines but preserves trailing newlines. This is necessary
    -- because we want to drop spaces based on how the closing @ line is
    -- indented but not on how the actual code block is indented.
    splitByLF :: Text -> [Text]
    splitByLF = L.unfoldr f . (T.cons '\n')
      where
        f :: Text -> Maybe (Text, Text)
        f txt | Just ('\n', rest) <- T.uncons txt =
                  Just (T.span (/= '\n') rest)
              | otherwise =
                  Nothing

    dropSpace :: Text -> Maybe Text
    dropSpace t =
      case T.uncons t of
        Nothing        -> Just ""
        Just (' ', t') -> Just t'
        _              -> Nothing

    cbText :: Parser Text
    cbText = P.scan False p
      where
        p :: Bool -> Char -> Maybe Bool
        p isLF c
          | isLF && c == '@'  = Nothing
          | isLF && isSpace c = Just isLF
          | otherwise         = Just (c == '\n')

instance Parsable Property where
  parse = Property . T.strip <$> ("prop>" *> P.takeWhile1 (/= '\n'))

instance Parsable Heading where
  parse =
    do level <- T.length <$> parser
       title <- parseInlines True =<< (horizSpaces *> nonEmptyLine)
       pure $ Heading level title
    where
      parser :: Parser Text
      parser = P.choice parsers

      parsers :: [Parser Text]
      parsers = P.string . flip T.replicate "=" <$> [6, 5..1]

instance Parsable BlockMath where
  parse =
    BlockMath <$> (P.string "\\[" *> takeUntil1 "\\]")

instance Parsable InlineMath where
  parse =
    InlineMath <$> rejectLF (P.string "\\(" *> takeUntil1 "\\)")

inlines :: Bool -> Parser Inlines
inlines collapseSpaces =
  Inlines . NE.fromList <$> P.many1' (inline collapseSpaces)

paragraph :: Parser Inlines
paragraph =
  parseInlines True =<< (T.intercalate "\n" <$> P.many1' nonEmptyLine)

paragraphThatStartsWithMarkdownLink :: Parser Inlines
paragraphThatStartsWithMarkdownLink =
  do i  <- Inlines . (:| []) <$> markdownLink
     is <- paragraph
     pure $ i <> is

-- The boolean argument indicates whether consecutive white spaces should
-- be collapsed or not.
inline :: Bool -> Parser Inline
inline collapseSpaces =
  P.choice [ IMonospaced <$> parse
           , IEmphasis   <$> parse
           , IBold       <$> parse
           , IAnchor     <$> parse
           , IIdentifier <$> parse
           , IModLink    <$> parse
           , markdownLink
           , hyperlink
           , IPicture    <$> parse
           , IMath       <$> parse
           , IWord       <$> parse
           , if collapseSpaces then
               ISpace <$> parse
             else
               empty -- Skip this entry.
           , IWord . Word . T.singleton <$> P.anyChar
           ]

instance Parsable Monospaced where
  parse =
    do _  <- P.char '@'
       is <- parseInlines True =<< takeWhile1_ (/= '@')
       _  <- P.char '@'
       pure . Monospaced $ is

instance Parsable Emphasis where
  parse =
    do _  <- P.char '/'
       is <- parseInlines True =<< rejectLF (takeWhile1_ (/= '/'))
       _  <- P.char '/'
       pure . Emphasis $ is

instance Parsable Bold where
  parse =
    do _  <- P.string "__"
       is <- parseInlines True =<< rejectLF (takeUntil1 "__")
       pure . Bold $ is

instance Parsable Anchor where
  parse =
    do _ <- P.char '#'
       a <- takeWhile1_ (\c -> c /= '#' && not (isSpace c))
       _ <- P.char '#'
       pure . Anchor $ a

instance Parsable Identifier where
  parse =
    do -- Optional namespace (type or value)
       ns <- parse

       -- Opening tick
       void tick

       -- Identifier or operator, which might contain tick symbols
       -- itself. We try to match as many valid Haskell
       -- identifiers/operators as possible.
       ident <- wrapped

       -- Closing tick
       void tick

       pure $ Identifier ns ident
    where
      tick :: Parser Char
      tick = P.char '\'' <|> P.char '`'

      -- Parse a wrapped, possibly qualified, operator or identifier.
      wrapped :: Parser Text
      wrapped =
        P.choice [ tuples
                 , parenthesised
                 , backticked
                 , general False
                 ]

      -- (), (,), (,,), ...
      tuples :: Parser Text
      tuples =
        fst <$> ( P.match $
                  do void $ P.char '('
                     P.skipWhile (== ',')
                     void $ P.char ')' )

      -- An identifier or an operator in parentheses like '(<$>)'.
      parenthesised :: Parser Text
      parenthesised =
        fst <$> ( P.match $
                  do void $ P.char '('
                     void $ general False
                     void $ P.char ')' )

      -- An identifier in backticks like '`isPrefixOf`'.
      backticked :: Parser Text
      backticked =
        fst <$> ( P.match $
                  do void $ P.char '`'
                     void $ general False
                     void $ P.char '`' )

      -- An unadorned, possibly qualified, operator or identifier.
      general :: Bool -- ^Refuse inputs starting with operators
              -> Parser Text
      general = (fst <$>) . P.match . general'

      general' :: Bool -> Parser ()
      general' !identOnly =
        -- Input starts with an identifier (either just an identifier, or a
        -- module qual).
        ( do P.skip      $ \c -> c == '_' || isAlpha    c
             P.skipWhile $ \c -> c == '_' || isAlphaNum c
             P.skipWhile (== '#')
             -- Take a look at the next character. Since we expect that
             -- there will be a tick in the end, we can fail if we reach
             -- EOF at this point.
             c <- P.peekChar'
             case c of
               -- Occurence of a backtick means the identifier definitely
               -- ends here, because they cannot legally contain any
               -- backticks. The same goes for parentheses.
               _ | c == '`' || c == ')' -> pure ()

               -- Occurence of a period means the identifier definitely
               -- does NOT end here, because they cannot legally end with a
               -- period.
               '.' -> P.anyChar *> general' False

               -- Occurence of a single quote is the worst of all. It
               -- either means the end of the identifier or is a part of
               -- it. We first explore the latter case, and if it fails we
               -- fall back to the former case.
               '\'' ->
                 (P.skipWhile (== '\'') *> general' True)
                 <|>
                 -- In the fallback case we don't even consume this quote.
                 pure ()

               -- Occurence of any other characters means that we are getting
               -- something not a valid identifier at this point.
               _ -> fail "invalid identifier"
        )
        <|>
        -- Input starts with an operator.
        ( do when identOnly $
               fail "an operator occured unexpectedly"
             P.skip      isSymbolChar
             P.skipWhile isSymbolChar
        )
        -- If it starts with anything else, then it's definitely not a
        -- valid identifier or an operator.

instance Parsable Namespace where
  parse =
    P.choice [ P.char 'v' *> pure Value
             , P.char 't' *> pure Type
             ,               pure None
             ]

markdownLink :: Parser Inline
markdownLink =
  do label <- markdownLinkText
     modLink label <|> urlLink label
  where
    modLink :: Inlines -> Parser Inline
    modLink label =
      do _    <- P.char '('
         _    <- horizSpaces
         name <- P.char '\"' *> moduleName <* P.char '\"'
         _    <- horizSpaces
         _    <- P.char ')'
         pure . IModLink $ ModLink name (Just label)

    urlLink :: Inlines -> Parser Inline
    urlLink label =
      do target <- markdownLinkTarget
         pure . IHyperlink $ Hyperlink target (Just label)

markdownLinkText :: Parser Inlines
markdownLinkText =
  parseInlines True =<<
  (T.strip <$> (P.char '[' *> takeWhile1_ (/= ']') <* P.char ']'))

markdownLinkTarget :: Parser URI
markdownLinkTarget = whitespace *> url
  where
    whitespace :: Parser ()
    whitespace = void horizSpaces <*
                 P.option Nothing (Just <$> (P.char '\n' *> horizSpaces))

    url :: Parser URI
    url =
      do txt <- rejectWhitespace (removeEscapes <$> (P.char '(' *> takeWhile1_ (/= ')')))
         maybe (fail "malformed URI") pure $
           parseURIReference (T.unpack txt)

    rejectWhitespace :: MonadPlus m => m Text -> m Text
    rejectWhitespace = mfilter (T.all (not . isSpace))

hyperlink :: Parser Inline
hyperlink = IHyperlink <$> (angleBracketLink <|> autoLink)
  where
    angleBracketLink :: Parser Hyperlink
    angleBracketLink =
      do txt <- rejectLF (P.char '<' *> takeWhile1_ (/= '>') <* P.char '>')
         mkLabeled Hyperlink
           (maybe (fail "malformed URI") pure
             . parseURIReference
             . T.unpack)
           (parseInlines True)
           txt

    -- Look for URL-like things to automatically turn into hyperlinks even
    -- if they aren't marked as so.
    autoLink :: Parser Hyperlink
    autoLink =
      do scheme <- P.choice ["http://", "https://", "ftp://"]
         -- NOTE: The original implementation of Haddock treats [,.!?] as
         -- not a part of URL if it's the last character of the possible
         -- URL. We think it's questionable at best, because URLs can
         -- legally end with these characters.
         rest   <- P.takeWhile1 (not . isSpace)
         uri    <- maybe (fail "malformed URI") pure $
                   parseURIReference (T.unpack (scheme <> rest))
         pure $ Hyperlink uri Nothing

removeEscapes :: Text -> Text
removeEscapes = T.unfoldr go
  where
    go :: Text -> Maybe (Char, Text)
    go xs = case T.uncons xs of
              Just ('\\', ys) -> T.uncons ys
              unconsed        -> unconsed

instance Parsable ModLink where
  parse = flip ModLink Nothing <$>
          (P.char '"' *> moduleName <* P.char '"')

moduleName :: Parser Text
moduleName = modId `maybeFollowedBy` anchor
  where
    modId :: Parser Text
    modId = T.intercalate "." <$> P.sepBy1' conId (P.char '.')

    maybeFollowedBy :: Monoid a => Parser a -> Parser a -> Parser a
    maybeFollowedBy pfx sfx =
      (\a -> maybe a (a <>)) <$> pfx <*> P.option Nothing (Just <$> sfx)

    conId :: Parser Text
    conId = T.cons
            <$> P.satisfy (\c -> isAlpha c && isUpper c)
            <*> P.takeWhile (\c -> isAlphaNum c || c == '_')

    anchor :: Parser Text
    anchor = (<>)
             <$> ("#" <|> "\\#")
             <*> P.takeWhile (\c -> c /= '"' && not (isSpace c))

instance Parsable Picture where
  parse =
    do txt <- rejectLF (P.string "<<" *> takeUntil1 ">>")
       mkLabeled Picture
         (maybe (fail "malformed URI") pure
          . parseURIReference
          . T.unpack)
         pure
         txt

instance Parsable Word where
  parse = Word . removeEscapes <$> takeWhile1_ p
    where
      p :: Char -> Bool
      p = P.notInClass "_/<@\"&'`#[ \t\n"

instance Parsable Space where
  parse = P.takeWhile1 p *> pure Space
    where
      p :: Char -> Bool
      p ' '  = True
      p '\t' = True
      p '\n' = True
      p _    = False

parseInlines :: MonadFail m => Bool -> Text -> m Inlines
parseInlines collapseSpaces txt =
  case P.parseOnly (inlines collapseSpaces <* P.endOfInput) txt of
    Left msg -> fail $ "Haddock parser should never fail: " <> msg
    Right is -> pure is

parseBlocks :: MonadFail m => Text -> m Haddock
parseBlocks txt =
  case P.parseOnly (parse <* P.endOfInput) txt of
    Left msg -> fail $ "Haddock parser should never fail: " <> msg
    Right h  -> pure h

parseHaddock :: HasCallStack => Text -> Haddock
parseHaddock txt =
  case P.parseOnly (parse <* P.endOfInput) txt of
    Left msg -> error $ "Haddock parser should never fail: " <> msg
    Right h  -> h
