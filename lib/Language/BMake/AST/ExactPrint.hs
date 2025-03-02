{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Language.BMake.AST.ExactPrint
  ( -- * AST variant tag
    ExactPrint

    -- * AST annotations
  , Whitespace(..)
  , EndOfLine(..)

    -- * Parsing and exact-printing
  , parseMakefile
  , exactPrintMakefile
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable1 (foldMap1', foldlMap1')
import Data.String (IsString)
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Attoparsec.Text.Lazy qualified as AL
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Language.BMake.AST.Extension
import Language.BMake.AST.Parse (Parsable(..))
import Language.BMake.AST.Pretty (Pretty(..))
import Language.BMake.AST.Types
import Prelude hiding (exp)
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PT

-- |The type @'Makefile' 'ExactPrint'@ is a variant of Makefile AST that is
-- obtained by parsing an actual Makefile. It contains all the information
-- the original Makefile has, including whitespaces, and is supposed to be
-- pretty-printable back to exactly the same Makefile.
data ExactPrint
  deriving (Data)

type instance XComment     ExactPrint   = ()
type instance XBlank       ExactPrint   = (Whitespace, EndOfLine)
type instance XAssignment  ExactPrint   = (Whitespace, Whitespace, EndOfLine)
type instance XValue       ExactPrint   = Whitespace
type instance XDependency  ExactPrint   = (Whitespace, Whitespace, EndOfLine)
type instance XShellCmd    ExactPrint   = ([Blank ExactPrint], EndOfLine)
type instance XInclude     ExactPrint   = (Whitespace, Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XMessage     ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XExport      ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XExportAll   ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XUnexportEnv ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XUndef       ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XConditional ExactPrint   = ()
type instance XElse        ExactPrint   = (Whitespace, Whitespace, Whitespace)
type instance XEndIf       ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XIf          ExactPrint   = (Whitespace, Whitespace, Whitespace)
type instance XIfdef       ExactPrint   = (Whitespace, Whitespace, Whitespace)
type instance XIfmake      ExactPrint   = (Whitespace, Whitespace, Whitespace)
type instance XNot         ExactPrint   = ()
type instance XAnd         ExactPrint   = ()
type instance XOr          ExactPrint   = ()
type instance XExpr        ExactPrint   = (Whitespace, Whitespace)
-- |A logical expression in parentheses.
type instance XExpLE       ExactPrint a = (Whitespace, Whitespace, LogicalExpr ExactPrint a)
type instance XECompare    ExactPrint   = Whitespace
type instance XFor         ExactPrint   = (Whitespace, Whitespace, Whitespace, Whitespace)
type instance XEndFor      ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XBreak       ExactPrint   = (Whitespace, Whitespace, Whitespace, EndOfLine)

newtype Whitespace = Whitespace Text
  deriving stock   (Data, Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)

parseWs1 :: Parser Whitespace
parseWs1 =
  do s <- parse
     when (nullWs s) $
       fail "whitespace is mandatory here"
     pure s
  where
    nullWs :: Whitespace -> Bool
    nullWs = coerce T.null

instance Parsable Whitespace where
  parse = Whitespace . fst <$> AL.match go
    where
      go :: Parser ()
      go =
        do cm <- AL.peekChar
           case cm of
             Nothing   -> pure () -- reached EOF
             Just '\\' ->
               -- Escaped newline is a whitespace. Anything else, including
               -- escaped backslash, does not count as a whitespace.
               (AL.string "\\\n" *> go)
               <|>
               pure ()
             Just ' '  -> AL.take 1 *> go
             Just '\t' -> AL.take 1 *> go
             Just _    -> pure ()

instance Pretty Whitespace where
  pretty _ (Whitespace s) = PP.pretty s

data EndOfLine = EOL | EOF
  deriving (Data, Eq, Show)

instance Parsable EndOfLine where
  parse = AL.choice [ AL.char '\n'  >> pure EOL
                    , AL.endOfInput >> pure EOF
                    ]

instance Pretty EndOfLine where
  pretty _ EOL = PP.hardline
  pretty _ EOF = mempty

instance Parsable (Makefile ExactPrint) where
  parse = Makefile <$> AL.many' parse

instance Pretty (Makefile ExactPrint) where
  pretty ctx = mconcat . (pretty ctx <$>) . blocks

instance Parsable (Comment ExactPrint) where
  parse = do _ <- AL.char '#'
             c <- AL.takeTill (== '\n')
             pure (Comment () c)

-- |Comments in the exact-print AST are pretty-printed with nothing between
-- @#@ and the comment text.
instance Pretty (Comment ExactPrint) where
  pretty _ (Comment _ c)
    = PP.pretty '#' <> PP.pretty c

parseComment :: Parser (Maybe (Comment ExactPrint))
parseComment = AL.option Nothing (Just <$> parse)

pprComment :: Maybe (Comment ExactPrint) -> Doc ann
pprComment = foldMap (pretty ())

instance Parsable (Block ExactPrint) where
  parse = AL.choice [ BBlank      <$> parse
                    , BAssignment <$> parse
                    , BRule       <$> parse
                    , BDirective  <$> parse
                    ]

instance Pretty (Block ExactPrint) where
  pretty _ b
    | BBlank      bl <- b = pretty () bl
    | BAssignment as <- b = pretty () as
    | BRule       ru <- b = pretty () ru
    | BDirective  di <- b = pretty () di

instance Parsable (Blank ExactPrint) where
  parse = do end <- AL.atEnd
             when end $
               -- Prevent an infinite loop.
               fail "Blank should not match EOL"
             ts  <- parse
             c   <- parseComment
             e   <- parse
             pure (Blank (ts, e) c)

instance Pretty (Blank ExactPrint) where
  pretty _ (Blank (ts, e) c) =
    mconcat [ pretty () ts
            , pprComment c
            , pretty () e
            ]

instance Parsable (Assignment ExactPrint) where
  parse = do s0 <- parse
             (v, op) <- parseVarOp
             s1 <- parse
             vs <- AL.many' parse
             c  <- parseComment
             e  <- parse
             pure $ Assignment (s0, s1, e) v op vs c
    where
      -- The syntax for variable assignments is ambiguous. Variable names
      -- are not disallowed to contain symbols like '+', so we must
      -- interpret "C++=/usr/bin/CC" as "C+ += /usr/bin/CC". This means we
      -- have to do a non-greedy match, which isn't exactly easy with
      -- attoparsec. The fact that variable names can also contain
      -- modifiers (like "${FOO:x=y} = BAR") further complicates things.
      parseVarOp :: Parser (Value ExactPrint, AssignmentOp)
      parseVarOp = do (nameB, ws, op) <- go 0 mempty
                      let name = TL.toStrict . TLB.toLazyText $ nameB
                      if T.null name
                        then fail "empty variable name"
                        else pure (Value ws name, op)
        where
          go :: Int -> Builder -> Parser (Builder, Whitespace, AssignmentOp)
          go level nameB =
            do c <- AL.peekChar'
               -- No lines can ever end with a variable name in the bmake
               -- syntax, so it's fine to fail here if we reach EOF before
               -- successfully peeking a character.
               case c of
                 _ | c == '(' || c == '{' ->
                       appendAndGo (level + 1) nameB

                   | c == ')' || c == '}' ->
                       appendAndGo (level - 1) nameB

                   | level /= 0 ->
                       appendAndGo level nameB

                   | c == ' ' || c == '\t' ->
                       -- Found a whitespace. It's definitely the end of
                       -- variable name.
                       do ws <- parse
                          op <- parse
                          pure (nameB, ws, op)

                   | c == '=' ->
                       -- Found an equal sign before any other relevant
                       -- symbols. It's the end of the variable name.
                       AL.anyChar *> pure (nameB, mempty, Set)

                   | c == '+' || c == '?' || c == ':' || c == '!' ->
                       -- These symbols may be a part of an assignment
                       -- op. Maybe not.
                       ( do op <- parse
                            pure (nameB, mempty, op)
                       )
                       <|> appendAndGo level nameB

                   | otherwise ->
                       appendAndGo level nameB

          appendAndGo :: Int -> Builder -> Parser (Builder, Whitespace, AssignmentOp)
          appendAndGo level nameB =
            do c <- AL.anyChar
               go level (nameB <> TLB.singleton c)

instance Pretty (Assignment ExactPrint) where
  pretty _ (Assignment {..})
    | (s0, s1, e) <- aExt =
        mconcat [ pretty () s0
                , pretty () aVar
                , pretty () aOp
                , pretty () s1
                , mconcat $ pretty () <$> aValues
                , pprComment aComment
                , pretty () e
                ]

-- Parsing a value isn't easy at all. Sigh... The bmake syntax is
-- crazy. Normally the '#' symbol begins a comment, but it can also appear
-- in a range modifier e.g. ${var:[#]}
instance Parsable (Value ExactPrint) where
  parse = do val <- AL.scan '\0' go
             when (T.null val) $
               fail "empty value"
             s   <- parse
             pure $ Value s val
    where
      go :: Char -- ^last character
         -> Char -- ^current character
         -> Maybe Char
      go '\\' ' '  = Just ' '  -- Escaped whitespace.
      go _    ' '  = Nothing   -- Unescaped whitespace ends a value.
      go '\\' '\t' = Just '\t'
      go _    '\t' = Nothing
      go '\\' '\n' = Just '\n'
      go _    '\n' = Nothing
      go '\\' '#'  = Just '#'
      go '['  '#'  = Just '#'  -- This isn't perfect but is what ParseRawLine() in parse.c does.
      go _    '#'  = Nothing   -- Unescaped hash ends a value and starts a comment.
      go '\\' '\\' = Just '\0' -- Escaped backslash loses its special ability.
      go _    c    = Just c

instance Pretty (Value ExactPrint) where
  pretty _ (Value s val) = pprValue val <> pretty () s
    where
      -- And of course pretty-printing values isn't easy, especially if one
      -- cares about efficiency, which we do.
      pprValue :: Text -> Doc ann
      pprValue = PP.pretty . TLB.toLazyText . go

      go :: Text -> Builder
      go txt =
        case T.uncons txt of
          Nothing      -> mempty
          Just (c, cs) -> go' (escape '\0' c) c cs

      go' :: Builder -> Char -> Text -> Builder
      go' acc c0 txt =
        case T.uncons txt of
          Nothing      -> acc
          Just (c, cs) -> go' (acc <> escape c0 c) c cs

      escape :: Char -> Char -> Builder
      escape _   ' '  = "\\ "
      escape _   '\t' = "\\\t"
      escape _   '\n' = "\\\n"
      escape '[' '#'  = TLB.singleton '#'
      escape _   '#'  = "\\#"
      escape _   '\\' = "\\\\"
      escape _   c    = TLB.singleton c

instance Parsable (Rule ExactPrint) where
  parse = do dep  <- parse
             cmds <- AL.many' parse
             pure $ Rule dep cmds

instance Pretty (Rule ExactPrint) where
  pretty _ (Rule {..}) =
    mconcat [ pretty () rDependency
            , mconcat $ pretty () <$> rCommands
            ]

instance Parsable (Dependency ExactPrint) where
  parse = do s0 <- parse
             ts <- NE.fromList <$> AL.many1' parse
             ty <- parse
             s1 <- parse
             ss <- AL.many' parse
             c  <- parseComment
             e  <- parse
             pure $ Dependency (s0, s1, e) ts ty ss c

instance Pretty (Dependency ExactPrint) where
  pretty _ (Dependency {..})
    | (s0, s1, e) <- dExt =
        mconcat [ pretty () s0
                , foldMap1' (pretty ()) dTargets
                , pretty () dType
                , pretty () s1
                , mconcat $ pretty () <$> dSources
                , pprComment dComment
                , pretty () e
                ]

instance Parsable (ShellCmd ExactPrint) where
  parse = do bs  <- AL.many' parse
             _   <- AL.char '\t'
             ms  <- AL.many' parse
             cmd <- parse
             com <- parseComment
             e   <- parse
             pure $ ShellCmd (bs, e) ms cmd com

instance Pretty (ShellCmd ExactPrint) where
  pretty _ (ShellCmd {..})
    | (bs, e) <- sExt =
        mconcat [ mconcat $ pretty () <$> bs
                , PP.pretty '\t'
                , mconcat $ pretty () <$> sModes
                , pretty () sCommand
                , pprComment sComment
                , pretty () e
                ]

instance Parsable (Directive ExactPrint) where
  parse = AL.choice [ DInclude     <$> parse
                    , DMessage     <$> parse
                    , DExport      <$> parse
                    , DExportAll   <$> parse
                    , DUnexportEnv <$> parse
                    , DUndef       <$> parse
                    , DConditional <$> parse
                    , DFor         <$> parse
                    , DBreak       <$> parse
                    ]

instance Pretty (Directive ExactPrint) where
  pretty _ d
    | DInclude     inc <- d = pretty () inc
    | DMessage     msg <- d = pretty () msg
    | DExport      exp <- d = pretty () exp
    | DExportAll   exa <- d = pretty () exa
    | DUnexportEnv uxe <- d = pretty () uxe
    | DUndef       und <- d = pretty () und
    | DConditional con <- d = pretty () con
    | DFor         for <- d = pretty () for
    | DBreak       brk <- d = pretty () brk

instance Parsable (Include ExactPrint) where
  parse = do s0   <- parse
             _    <- AL.char '.'
             s1   <- parse
             mode <- parse
             s2   <- parse
             loc  <- AL.choice [ AL.char '<' *> pure System
                               , AL.char '"' *> pure User
                               ]
             let close = case loc of
                           System -> '>'
                           User   -> '"'
             path <- AL.takeTill (== close)
             _    <- AL.char close
             s3   <- parse
             com  <- parseComment
             e    <- parse
             pure $ Include (s0, s1, s2, s3, e) mode loc path com

instance Pretty (Include ExactPrint) where
  pretty _ (Include {..})
    | (s0, s1, s2, s3, e) <- iExt =
        mconcat [ pretty () s0
                , PP.pretty '.'
                , pretty () s1
                , pretty () iMode
                , pretty () s2
                , case iLoc of
                    System -> PP.pretty '<'
                    User   -> PP.pretty '"'
                , PP.pretty iPath
                , case iLoc of
                    System -> PP.pretty '>'
                    User   -> PP.pretty '"'
                , pretty () s3
                , pprComment iComment
                , pretty () e
                ]

instance Parsable (Message ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             ty  <- AL.choice [ AL.string "info"    *> pure Info
                              , AL.string "warning" *> pure Warning
                              , AL.string "error"   *> pure Error
                              ]
             s2  <- parse
             msg <- parse
             com <- parseComment
             e   <- parse
             pure $ Message (s0, s1, s2, e) ty msg com

instance Pretty (Message ExactPrint) where
  pretty _ (Message {..})
    | (s0, s1, s2, e) <- msgExt =
        mconcat [ pretty () s0
                , PP.pretty '.'
                , pretty () s1
                , case msgType of
                    Info    -> "info"
                    Warning -> "warning"
                    Error   -> "error"
                , pretty () s2
                , pretty () msgText
                , pprComment msgComment
                , pretty () e
                ]

instance Parsable (Export ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             way <- AL.choice [ AL.string "export-env"     *> pure ExpEnv
                              , AL.string "export-literal" *> pure ExpLit
                              , AL.string "export"         *> pure Exp
                              , AL.string "unexport"       *> pure Unexp
                              ]
             s2  <- parse
             vs  <- AL.many' parse
             com <- parseComment
             e   <- parse
             pure $ Export (s0, s1, s2, e) way vs com

instance Pretty (Export ExactPrint) where
  pretty _ (Export {..})
    | (s0, s1, s2, e) <- expExt =
        mconcat [ pretty () s0
                , PP.pretty '.'
                , pretty () s1
                , case expWay of
                    ExpEnv -> "export-env"
                    ExpLit -> "export-lit"
                    Exp    -> "export"
                    Unexp  -> "unexport"
                , pretty () s2
                , mconcat $ pretty () <$> expVars
                , pprComment expComment
                , pretty () e
                ]

instance Parsable (ExportAll ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "export-all"
             s2  <- parse
             com <- parseComment
             e   <- parse
             pure $ ExportAll (s0, s1, s2, e) com

instance Pretty (ExportAll ExactPrint) where
  pretty _ (ExportAll (s0, s1, s2, e) com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "export-all"
            , pretty () s2
            , pprComment com
            , pretty () e
            ]

instance Parsable (UnexportEnv ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "unexport-env"
             s2  <- parse
             com <- parseComment
             e   <- parse
             pure $ UnexportEnv (s0, s1, s2, e) com

instance Pretty (UnexportEnv ExactPrint) where
  pretty _ (UnexportEnv (s0, s1, s2, e) com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "unexport-env"
            , pretty () s2
            , pprComment com
            , pretty () e
            ]

instance Parsable (Undef ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "undef"
             s2  <- parse
             vs  <- AL.many' parse
             com <- parseComment
             e   <- parse
             pure $ Undef (s0, s1, s2, e) vs com

instance Pretty (Undef ExactPrint) where
  pretty _ (Undef (s0, s1, s2, e) vs com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "undef"
            , pretty () s2
            , mconcat $ pretty () <$> vs
            , pprComment com
            , pretty () e
            ]

instance Parsable (Conditional ExactPrint) where
  parse = do branches <- parseBranches
             else_    <- AL.option Nothing (Just <$> parse)
             end      <- parse
             pure $ Conditional () branches else_ end
    where
      parseBranches :: Parser (NonEmpty (CondBranch ExactPrint))
      parseBranches =
        do b  <- parseCondBranch True
           bs <- AL.many' $ parseCondBranch False
           pure $ NE.fromList (b:bs)

instance Pretty (Conditional ExactPrint) where
  pretty _ (Conditional {..}) =
    mconcat [ pprBranches condBranches
            , foldMap (pretty ()) condElse
            , pretty () condEnd
            ]
    where
      pprBranches :: NonEmpty (CondBranch ExactPrint) -> Doc ann
      pprBranches (br :| brs) =
        pretty True br <> mconcat (pretty False <$> brs)

instance Parsable (Else ExactPrint) where
  parse = do s0   <- parse
             _    <- AL.char '.'
             s1   <- parse
             _    <- AL.string "else"
             s2   <- parse
             com  <- parseComment
             -- No need to save EndOfLine because no Makefiles can legally
             -- end with ".else".
             _    <- AL.char '\n'
             body <- parse
             pure $ Else (s0, s1, s2) com body

instance Pretty (Else ExactPrint) where
  pretty _ (Else (s0, s1, s2) com body) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "else"
            , pretty () s2
            , pprComment com
            , PP.hardline
            , pretty () body
            ]

instance Parsable (EndIf ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "endif"
             s2  <- parse
             com <- parseComment
             e   <- parse
             pure $ EndIf (s0, s1, s2, e) com

instance Pretty (EndIf ExactPrint) where
  pretty _ (EndIf (s0, s1, s2, e) com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "endif"
            , pretty () s2
            , pprComment com
            , pretty () e
            ]

-- |See 'parseCondition'.
parseCondBranch :: Bool -> Parser (CondBranch ExactPrint)
parseCondBranch isFirst =
  do cond <- parseCondition isFirst
     -- The Parsable instance for (Makefile a) eats up everything but
     -- unnested ".elif*", ".else", or ".endif".
     body <- parse
     pure $ CondBranch cond body

instance Pretty (CondBranch ExactPrint) where
  -- |Denotes whether this is the first branch in a chain.
  type Context (CondBranch ExactPrint) = Bool
  pretty isFirst (CondBranch cond body) =
    mconcat [ pretty isFirst cond
            , pretty () body
            ]

-- |@'parseCondition' isFirst@ parses a 'Condition' where @isFirst@ denotes
-- whether the condition is the first one in a chain (i.e. @.if@, @.ifdef@,
-- ...) or not (i.e. @.elif@, @.elifdef@, ...).
parseCondition :: Bool -> Parser (Condition ExactPrint)
parseCondition isFirst =
  do s0 <- parse
     _  <- AL.char '.'
     s1 <- parse
     unless isFirst . void
       $ AL.string "el"
     AL.choice [ AL.string "ifdef"   *> parseIfdef  s0 s1 True
               , AL.string "ifndef"  *> parseIfdef  s0 s1 False
               , AL.string "ifmake"  *> parseIfmake s0 s1 True
               , AL.string "ifnmake" *> parseIfmake s0 s1 False
               , AL.string "if"      *> parseIf     s0 s1
               ] <* AL.char '\n'
    where
      -- These don't explicitly save EndOfLine because no Makefiles can
      -- legally end with these.
      parseIf :: Whitespace -> Whitespace -> Parser (Condition ExactPrint)
      parseIf s0 s1 =
        do s2 <- parseWs1
           If (s0, s1, s2) <$> parse <*> parseComment

      parseIfdef :: Whitespace -> Whitespace -> Bool -> Parser (Condition ExactPrint)
      parseIfdef s0 s1 n =
        do s2 <- parseWs1
           Ifdef (s0, s1, s2) n <$> parse <*> parseComment

      parseIfmake :: Whitespace -> Whitespace -> Bool -> Parser (Condition ExactPrint)
      parseIfmake s0 s1 n =
        do s2 <- parseWs1
           Ifmake (s0, s1, s2) n <$> parse <*> parseComment

instance Pretty (Condition ExactPrint) where
  -- |Denotes whether the branch it's used by is the first one in a chain.
  type Context (Condition ExactPrint) = Bool
  pretty isFirst c
    | If     (s0, s1, s2)       le com <- c = pprCond s0 s1 "if"      s2 le com
    | Ifdef  (s0, s1, s2) True  le com <- c = pprCond s0 s1 "ifdef"   s2 le com
    | Ifdef  (s0, s1, s2) False le com <- c = pprCond s0 s1 "ifndef"  s2 le com
    | Ifmake (s0, s1, s2) True  le com <- c = pprCond s0 s1 "ifmake"  s2 le com
    | Ifmake (s0, s1, s2) False le com <- c = pprCond s0 s1 "ifnmake" s2 le com
    where
      pprCond :: (Pretty a, Context a ~ ())
              => Whitespace
              -> Whitespace
              -> Doc ann
              -> Whitespace
              -> LogicalExpr ExactPrint a
              -> Maybe (Comment ExactPrint)
              -> Doc ann
      pprCond s0 s1 label s2 le com =
        mconcat [ pretty () s0
                , PP.pretty '.'
                , pretty () s1
                , if isFirst then mempty else "el"
                , label
                , pretty () s2
                , pretty () le
                , pprComment com
                , PP.hardline
                ]

instance Parsable a => Parsable (LogicalExpr ExactPrint a) where
  -- Here we use the following grammar (in ABNF) to avoid infinite loops on
  -- left recursion. Hope it's equivalent to what bmake actually does:
  --
  --   logical-expr = not-expr
  --   not-expr     = ["!"] and-expr
  --   and-expr     = or-expr *("&&" or-expr)
  --   or-expr      = paren-expr *("||" paren-expr)
  --   paren-expr   = "(" logical-expr ")" / expr
  --
  parse = parse >>= parseLE
    where
      parseLE :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseLE = parseNot

      parseNot :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseNot s0 =
        ( do _  <- AL.char '!'
             le <- parseAnd s0
             pure $ Not () le
        )
        <|> parseAnd s0

      parseAnd :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseAnd s0 =
        do le  <- parseOr s0
           les <- AL.many' $ AL.string "&&" *> (parse >>= parseOr)
           if null les
             then pure le
             else pure . And () $ NE.fromList (le : les)

      parseOr :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseOr s0 =
        do le  <- parseParen s0
           les <- AL.many' $ AL.string "||" *> (parse >>= parseParen)
           if null les
             then pure le
             else pure . Or () $ NE.fromList (le : les)

      parseParen :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseParen s0 =
        ( do _  <- AL.char '('
             le <- parse >>= parseLE
             _  <- AL.char ')'
             s1 <- parse
             pure $ ExpLE (s0, s1, le)
        )
        <|>
        ( do expr <- parse
             s1   <- parse
             pure $ Expr (s0, s1) expr
        )

instance (Pretty a, Context a ~ ()) => Pretty (LogicalExpr ExactPrint a) where
  pretty _ le
    | Not   ()       le'  <- le = PP.pretty '!' <> pretty () le'
    | And   ()       les  <- le = foldlMap1' (pretty ()) ((. pretty ()) . (<>) . (<> "&&")) les
    | Or    ()       les  <- le = foldlMap1' (pretty ()) ((. pretty ()) . (<>) . (<> "||")) les
    | Expr  (s0, s1) expr <- le = mconcat [ pretty () s0
                                          , pretty () expr
                                          , pretty () s1
                                          ]
    | ExpLE (s0, s1, le') <- le = mconcat [ pretty () s0
                                          , PP.parens $ pretty () le'
                                          , pretty () s1
                                          ]

instance Parsable (Expr ExactPrint) where
  parse = AL.choice [ EDefined  <$> parseFunc "defined"
                    , EMake     <$> parseFunc "make"
                    , EEmpty    <$> parseFunc "empty"
                    , EExists   <$> parseFunc "exists"
                    , ETarget   <$> parseFunc "target"
                    , ECommands <$> parseFunc "commands"
                    , parseComp
                    ]
    where
      parseFunc :: Text -> Parser Text
      parseFunc name =
        do _   <- AL.string name
           _   <- AL.char '('
           arg <- parseArg
           _   <- AL.char ')'
           pure arg

      parseArg :: Parser Text
      parseArg = TL.toStrict . TLB.toLazyText <$> go 0 mempty
        where
          go :: Int -> Builder -> Parser Builder
          go level argB =
            do c <- AL.peekChar'
               -- Makefiles aren't allowed to reach EOF here, so it's fine
               -- to use peekChar'
               case c of
                 '(' ->
                   appendAndGo (level + 1) argB

                 ')' | level == 0 -> pure argB -- Argument ends here.
                     | otherwise  -> appendAndGo (level - 1) argB

                 _ ->
                   appendAndGo level argB

          appendAndGo :: Int -> Builder -> Parser Builder
          appendAndGo level argB =
            do c <- AL.anyChar
               go level (argB <> TLB.singleton c)

      parseComp :: Parser (Expr ExactPrint)
      parseComp =
        do lhs <- parse
           -- No whitespace parsing here, as LHS already contains that.
           parseCompRHS lhs <|> pure (ECompare mempty lhs Nothing)

      parseCompRHS :: Value ExactPrint -> Parser (Expr ExactPrint)
      parseCompRHS lhs =
        do op  <- parse
           s0  <- parse
           rhs <- parse
           pure $ ECompare s0 lhs (Just (op, rhs))

instance Pretty (Expr ExactPrint) where
  pretty _ e
    | EDefined  arg <- e = pprFunc "defined"  arg
    | EMake     arg <- e = pprFunc "make"     arg
    | EEmpty    arg <- e = pprFunc "empty"    arg
    | EExists   arg <- e = pprFunc "exists"   arg
    | ETarget   arg <- e = pprFunc "target"   arg
    | ECommands arg <- e = pprFunc "commands" arg
    | ECompare {..} <- e =
        let s0          = eCmpExt
            f (op, rhs) = mconcat [ pretty () op
                                  , pretty () s0
                                  , pretty () rhs
                                  ]
        in
          pretty () eCmpLHS <> foldMap f eCmpRHS
    where
      pprFunc :: Doc ann -> Text -> Doc ann
      pprFunc label arg =
        label <> PP.parens (PP.pretty arg)

instance Parsable (ForLoop ExactPrint) where
  parse = ForLoop <$> parse <*> parse <*> parse

instance Pretty (ForLoop ExactPrint) where
  pretty _ (ForLoop for body end) =
    mconcat [ pretty () for
            , pretty () body
            , pretty () end
            ]

instance Parsable (For ExactPrint) where
  parse = do s0       <- parse
             _        <- AL.char '.'
             s1       <- parse
             _        <- AL.string "for"
             s2       <- parse
             (vs, s3) <- parseVarsIn
             expr     <- parse
             com      <- parseComment
             _        <- AL.char '\n'
             pure $ For (s0, s1, s2, s3) vs expr com
    where
      parseVarsIn :: Parser (NonEmpty (Value ExactPrint), Whitespace)
      parseVarsIn =
        do v        <- parse
           (vs, s3) <- go
           pure ((v :| vs), s3)

      go :: Parser ([Value ExactPrint], Whitespace)
      go = ( do _  <- AL.string "in"
                s3 <- parse
                pure ([], s3)
           )
           <|>
           ( do v        <- parse
                (vs, s3) <- go
                pure ((v:vs), s3)
           )

instance Pretty (For ExactPrint) where
  pretty _ (For {..})
    | (s0, s1, s2, s3) <- forExt =
        mconcat [ pretty () s0
                , PP.pretty '.'
                , pretty () s1
                , "for"
                , pretty () s2
                , foldMap1' (pretty ()) forVars
                , "in"
                , pretty () s3
                , pretty () forExpr
                , pprComment forComment
                , PP.hardline
                ]

instance Parsable (EndFor ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "endfor"
             s2  <- parse
             com <- parseComment
             e   <- parse
             pure $ EndFor (s0, s1, s2, e) com

instance Pretty (EndFor ExactPrint) where
  pretty _ (EndFor (s0, s1, s2, e) com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "endfor"
            , pretty () s2
            , pprComment com
            , pretty () e
            ]

instance Parsable (Break ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "break"
             s2  <- parse
             com <- parseComment
             e   <- parse
             pure $ Break (s0, s1, s2, e) com

instance Pretty (Break ExactPrint) where
  pretty _ (Break (s0, s1, s2, e) com) =
    mconcat [ pretty () s0
            , PP.pretty '.'
            , pretty () s1
            , "break"
            , pretty () s2
            , pprComment com
            , pretty () e
            ]

parseMakefile :: TL.Text -> Either String (Makefile ExactPrint)
parseMakefile = AL.parseOnly (parse <* AL.endOfInput)

exactPrintMakefile :: Makefile ExactPrint -> TL.Text
exactPrintMakefile = PT.renderLazy . PP.layoutCompact . pretty ()
