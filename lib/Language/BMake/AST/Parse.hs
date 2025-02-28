{-# LANGUAGE OverloadedStrings #-}
module Language.BMake.AST.Parse
  ( Parsable(..)
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Attoparsec.Text.Lazy qualified as AL
import Language.BMake.AST.Types
import Prelude hiding (Ordering(..))


class Parsable a where
  parse :: Parser a

instance Parsable AssignmentOp where
  parse = AL.choice [ AL.char   '='  *> pure Set
                    , AL.string "+=" *> pure Append
                    , AL.string "?=" *> pure SetIfUndefined
                    , AL.string ":=" *> pure ExpandThenSet
                    , AL.string "!=" *> pure ExecThenSet
                    ]

-- We must do something similar to, but *not exactly the same as*, the
-- Parsable instance for Value, because spaces or tabs aren't special in
-- this context while newlines and hash symbols are special.
instance Parsable UnstructuredText where
  parse = UnstructuredText <$> AL.scan '\0' go
    where
      go :: Char -> Char -> Maybe Char
      go '\\' '\n' = Just '\n'
      go _    '\n' = Nothing
      go '\\' '#'  = Just '#'
      go '['  '#'  = Just '#'
      go _    '#'  = Nothing
      go '\\' '\\' = Just '\0'
      go _    c    = Just c

instance Parsable DependencyType where
  parse = ( AL.char ':' *> ( (AL.char ':' *> pure NoAccumulate)
                             <|> pure IfOlderThan )
          )
          <|> (AL.char '!' *> pure Always)

instance Parsable CommandMode where
  parse = AL.choice [ AL.char '@' *> pure NoEcho
                    , AL.char '+' *> pure Dry
                    , AL.char '-' *> pure IgnErr
                    ]

instance Parsable IncMode where
  parse = AL.choice [ AL.string "include"  *> pure Normal
                    , AL.string "sinclude" *> pure SMode
                    , AL.string "dinclude" *> pure DMode
                    ]

instance Parsable RelationalOp where
  parse = AL.choice [ AL.string "==" *> pure EQ
                    , AL.string "!=" *> pure NE
                    , AL.char '<' *> ((AL.char '=' *> pure LE) <|> pure LT)
                    , AL.char '>' *> ((AL.char '=' *> pure GE) <|> pure GT)
                    ]
