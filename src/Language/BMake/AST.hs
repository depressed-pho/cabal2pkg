module Language.BMake.AST
  ( -- * Types
    module Language.BMake.AST.Types

    -- * Construction
  , (#)
  , blank
  , (.=)
  , (.+=)
  , (.?=)
  , (.:=)
  , (.!=)
  , include
  , (?==)

    -- * Pretty-printing
  , prettyPrintAST
  ) where

import Data.Text (Text)
import Language.BMake.AST.Pretty (prettyPrintAST)
import Language.BMake.AST.Types
import Prelude hiding (Ordering(..))


infix 0 #
-- |Attach a comment to a block.
(#) :: Block -> Text -> Block
BBlank      b # c = BBlank      $ b { bComment = Just (Comment c) }
BAssignment a # c = BAssignment $ a { aComment = Just (Comment c) }
BRule       r # c = BRule       $ r { rComment = Just (Comment c) }
-- FIXME: This isn't quite correct. We should turn (#) into a method in
-- some class.
BDirective  d # _ = BDirective d

blank :: Block
blank = BBlank $ Blank Nothing

infix 1 .=
(.=) :: Variable -> [Text] -> Block
var .= tokens = BAssignment $ Assignment var Set tokens Nothing

infix 1 .+=
(.+=) :: Variable -> [Text] -> Block
var .+= tokens = BAssignment $ Assignment var Append tokens Nothing

infix 1 .?=
(.?=) :: Variable -> [Text] -> Block
var .?= tokens = BAssignment $ Assignment var SetIfUndefined tokens Nothing

infix 1 .:=
(.:=) :: Variable -> [Text] -> Block
var .:= tokens = BAssignment $ Assignment var ExpandThenSet tokens Nothing

infix 1 .!=
(.!=) :: Variable -> [Text] -> Block
var .!= tokens = BAssignment $ Assignment var ExecThenSet tokens Nothing

include :: Text -> Block
include = BDirective . DInclude . Include Normal User

infix 1 ?==
(?==) :: Text -> Text -> Expr
a ?== b = ECompare a (Just (EQ, b))
