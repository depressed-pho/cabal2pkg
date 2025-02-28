{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.BMake.AST.ExactPrint
  ( -- * AST variant tag
    ExactPrint

    -- * AST annotations
  , Whitespace(..)
  , EndOfLine(..)

    -- * Parsing and exact-printing
  , parseMakefile
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Data.Data (Data)
import Data.String (IsString)
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Attoparsec.Text.Lazy qualified as AL
import Data.List.NonEmpty (NonEmpty)
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
import Prettyprinter qualified as PP

-- |The type @'Makefile' 'ExactPrint'@ is a variant of Makefile AST that is
-- obtained by parsing an actual Makefile. It contains all the information
-- the original Makefile has, including whitespaces, and can be
-- pretty-printed back to exactly the same Makefile.
data ExactPrint
type instance XComment     ExactPrint = ()
type instance XBlank       ExactPrint = (Whitespace, EndOfLine)
type instance XAssignment  ExactPrint = (Whitespace, Whitespace, EndOfLine)
type instance XValue       ExactPrint = Whitespace
type instance XDependency  ExactPrint = (Whitespace, Whitespace, EndOfLine)
type instance XShellCmd    ExactPrint = ([Blank ExactPrint], EndOfLine)
type instance XInclude     ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XMessage     ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XExport      ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XExportAll   ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XUnexportEnv ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XUndef       ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XConditional ExactPrint = ()
type instance XElse        ExactPrint = (Whitespace, Whitespace, Whitespace)
type instance XEndIf       ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XIf          ExactPrint = (Whitespace, Whitespace)
type instance XIfdef       ExactPrint = (Whitespace, Whitespace)
type instance XIfmake      ExactPrint = (Whitespace, Whitespace)
type instance XNot         ExactPrint = (Bool, Whitespace, Whitespace)
type instance XOr          ExactPrint = (Bool, Whitespace, Whitespace)
type instance XAnd         ExactPrint = (Bool, Whitespace, Whitespace)
type instance XExpr        ExactPrint = (Bool, Whitespace, Whitespace)
type instance XECompare    ExactPrint = Whitespace
type instance XFor         ExactPrint = (Whitespace, Whitespace, Whitespace)
type instance XEndFor      ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)
type instance XBreak       ExactPrint = (Whitespace, Whitespace, Whitespace, EndOfLine)

newtype Whitespace = Whitespace Text
  deriving stock   (Data, Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)

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
  pretty _ EOL = PP.line
  pretty _ EOF = mempty

instance Parsable (Makefile ExactPrint) where
  parse = Makefile <$> AL.many' parse

instance Parsable (Comment ExactPrint) where
  parse = do _ <- AL.char '#'
             c <- AL.takeTill (== '\n')
             pure (Comment () c)

-- |Comments in the exact-print AST are pretty-printed with nothing between
-- @#@ and the comment text.
instance Pretty (Comment ExactPrint) where
  pretty _ (Comment _ c)
    = PP.pretty '#' <> PP.pretty c

optionalComment :: Parser (Maybe (Comment ExactPrint))
optionalComment = AL.option Nothing (Just <$> parse)

instance Parsable (Block ExactPrint) where
  parse = AL.choice [ BBlank      <$> parse
                    , BAssignment <$> parse
                    , BRule       <$> parse
                    , BDirective  <$> parse
                    ]

instance Parsable (Blank ExactPrint) where
  parse = do end <- AL.atEnd
             when end $
               -- Prevent an infinite loop.
               fail "Blank should not match EOL"
             ts  <- parse
             c   <- optionalComment
             e   <- parse
             pure (Blank (ts, e) c)

instance Pretty (Blank ExactPrint) where
  pretty _ (Blank (ts, e) c) =
    mconcat [ pretty () ts
            , foldMap (pretty ()) c
            , pretty () e
            ]

instance Parsable (Assignment ExactPrint) where
  parse = do s0 <- parse
             (v, op) <- parseVarOp
             s1 <- parse
             vs <- AL.many' parse
             c  <- optionalComment
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

-- Parsing a value isn't easy at all. Sigh... The bmake syntax is
-- crazy. Normally the '#' symbol begins a comment, but it can also appear
-- in a range modifier e.g. ${var:[#]}
instance Parsable (Value ExactPrint) where
  parse = do val <- AL.scan '\0' go
             when (T.null val) $
               fail "empty value"
             ts  <- parse
             pure $ Value ts val
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

instance Parsable (Rule ExactPrint) where
  parse = do dep  <- parse
             cmds <- AL.many' parse
             pure $ Rule dep cmds

instance Parsable (Dependency ExactPrint) where
  parse = do s0 <- parse
             ts <- AL.many1' parse
             ty <- parse
             s1 <- parse
             ss <- AL.many' parse
             c  <- optionalComment
             e  <- parse
             pure $ Dependency (s0, s1, e) ts ty ss c

instance Parsable (ShellCmd ExactPrint) where
  parse = do bs  <- AL.many' parse
             _   <- AL.char '\t'
             ms  <- AL.many' parse
             cmd <- parse
             com <- optionalComment
             e   <- parse
             pure $ ShellCmd (bs, e) ms cmd com

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
             com  <- optionalComment
             e    <- parse
             pure $ Include (s0, s1, s2, e) mode loc path com

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
             com <- optionalComment
             e   <- parse
             pure $ Message (s0, s1, s2, e) ty msg com

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
             com <- optionalComment
             e   <- parse
             pure $ Export (s0, s1, s2, e) way vs com

instance Parsable (ExportAll ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "export-all"
             s2  <- parse
             com <- optionalComment
             e   <- parse
             pure $ ExportAll (s0, s1, s2, e) com

instance Parsable (UnexportEnv ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "unexport-env"
             s2  <- parse
             com <- optionalComment
             e   <- parse
             pure $ UnexportEnv (s0, s1, s2, e) com

instance Parsable (Undef ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "undef"
             s2  <- parse
             vs  <- AL.many' parse
             com <- optionalComment
             e   <- parse
             pure $ Undef (s0, s1, s2, e) vs com

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

instance Parsable (Else ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "else"
             s2  <- parse
             com <- optionalComment
             -- No need to save EndOfLine because no Makefiles can legally
             -- end with ".else".
             _   <- AL.char '\n'
             mk  <- parse
             pure $ Else (s0, s1, s2) com mk

instance Parsable (EndIf ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "endif"
             s2  <- parse
             com <- optionalComment
             e   <- parse
             pure $ EndIf (s0, s1, s2, e) com

-- |See 'parseCondition'.
parseCondBranch :: Bool -> Parser (CondBranch ExactPrint)
parseCondBranch isFirst =
  do cond <- parseCondition isFirst
     -- The Parsable instance for (Makefile a) eats up everything but
     -- unnested ".elif*", ".else", or ".endif".
     mk   <- parse
     pure $ CondBranch cond mk

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
        If (s0, s1) <$> parse <*> optionalComment

      parseIfdef :: Whitespace -> Whitespace -> Bool -> Parser (Condition ExactPrint)
      parseIfdef s0 s1 n =
        Ifdef (s0, s1) n <$> parse <*> optionalComment

      parseIfmake :: Whitespace -> Whitespace -> Bool -> Parser (Condition ExactPrint)
      parseIfmake s0 s1 n =
        Ifmake (s0, s1) n <$> parse <*> optionalComment

instance Parsable a => Parsable (LogicalExpr ExactPrint a) where
  parse = parse >>= parseParen
    where
      parseParen :: Whitespace -> Parser (LogicalExpr ExactPrint a)
      parseParen s0 =
        ( do _  <- AL.char '('
             le <- parseLE
             _  <- AL.char ')'
             s1 <- parse
             pure $ le True s0 s1
        )
        <|>
        ( do le <- parseLE
             s1 <- parse
             pure $ le False s0 s1
        )

      parseLE :: Parser (Bool -> Whitespace -> Whitespace -> LogicalExpr ExactPrint a)
      parseLE = AL.choice [ parseNot
                          , parseOr
                          , parseAnd
                          , parseExpr
                          ]

      parseNot :: Parser (Bool -> Whitespace -> Whitespace -> LogicalExpr ExactPrint a)
      parseNot =
        do _  <- AL.char '!'
           le <- parse
           pure $ \hasParen s0 s1 -> Not (hasParen, s0, s1) le

      parseOr :: Parser (Bool -> Whitespace -> Whitespace -> LogicalExpr ExactPrint a)
      parseOr =
        do les <- NE.fromList <$> parse `AL.sepBy1'` (AL.string "||")
           pure $ \hasParen s0 s1 -> Or (hasParen, s0, s1) les

      parseAnd :: Parser (Bool -> Whitespace -> Whitespace -> LogicalExpr ExactPrint a)
      parseAnd =
        do les <- NE.fromList <$> parse `AL.sepBy1'` (AL.string "&&")
           pure $ \hasParen s0 s1 -> And (hasParen, s0, s1) les

      parseExpr :: Parser (Bool -> Whitespace -> Whitespace -> LogicalExpr ExactPrint a)
      parseExpr =
        do expr <- parse
           pure $ \hasParen s0 s1 -> Expr (hasParen, s0, s1) expr

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
      parseFunc :: Text -> Parser (Value ExactPrint)
      parseFunc name =
        do _   <- AL.string name
           _   <- AL.char '('
           arg <- parse
           _   <- AL.char ')'
           pure arg

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

instance Parsable (ForLoop ExactPrint) where
  parse = ForLoop <$> parse <*> parse <*> parse

instance Parsable (For ExactPrint) where
  parse = do s0   <- parse
             _    <- AL.char '.'
             s1   <- parse
             _    <- AL.string "for"
             s2   <- parse
             vs   <- AL.many1' parse
             expr <- parse
             com  <- optionalComment
             _    <- AL.char '\n'
             pure $ For (s0, s1, s2) vs expr com

instance Parsable (EndFor ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "endfor"
             s2  <- parse
             com <- optionalComment
             e   <- parse
             pure $ EndFor (s0, s1, s2, e) com

instance Parsable (Break ExactPrint) where
  parse = do s0  <- parse
             _   <- AL.char '.'
             s1  <- parse
             _   <- AL.string "break"
             s2  <- parse
             com <- optionalComment
             e   <- parse
             pure $ Break (s0, s1, s2, e) com

parseMakefile :: TL.Text -> Either String (Makefile ExactPrint)
parseMakefile = AL.parseOnly (parse <* AL.endOfInput)
