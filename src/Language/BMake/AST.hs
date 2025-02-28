module Language.BMake.AST
  ( -- * Types
    module Language.BMake.AST.Extension
  , module Language.BMake.AST.Types
  , PlainAST
  , ExactPrint

    -- * Parsing and pretty-printing
  , parseMakefile
  , prettyPrintMakefile
  ) where

import Language.BMake.AST.ExactPrint (ExactPrint, parseMakefile)
import Language.BMake.AST.Extension
import Language.BMake.AST.Plain (PlainAST)
import Language.BMake.AST.Pretty (prettyPrintMakefile)
import Language.BMake.AST.Types
import Prelude hiding (Ordering(..))
