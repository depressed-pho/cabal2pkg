{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.BMake.AST.Pretty
  ( Pretty(..)
  , prettyPrintMakefile
  ) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Language.BMake.AST.Types
import Prelude hiding (Ordering(..))
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PT

class Pretty a where
  type Context a
  type instance Context a = ()
  pretty  :: Context a -> a -> Doc ann

instance Pretty a => Pretty [a] where
  type Context [a] = Context a
  pretty ctx = PP.hsep . (pretty ctx <$>)

instance Pretty Text where
  pretty _ = PP.pretty

instance Pretty AssignmentOp where
  pretty _ Set            = PP.equals
  pretty _ Append         = "+="
  pretty _ SetIfUndefined = "?="
  pretty _ ExpandThenSet  = ":="
  pretty _ ExecThenSet    = "!="

instance Pretty UnstructuredText where
  pretty _ (UnstructuredText txt) = PP.pretty txt

instance Pretty DependencyType where
  pretty _ IfOlderThan  = PP.colon
  pretty _ Always       = PP.pretty '!'
  pretty _ NoAccumulate = "::"

instance Pretty CommandMode where
  pretty _ NoEcho = PP.pretty '@'
  pretty _ Dry    = PP.pretty '+'
  pretty _ IgnErr = PP.pretty '-'

instance Pretty IncMode where
  pretty _ Normal = "include"
  pretty _ SMode  = "sinclude"
  pretty _ DMode  = "dinclude"

instance Pretty RelationalOp where
  pretty _ EQ = "=="
  pretty _ NE = "!="
  pretty _ LT = PP.langle
  pretty _ LE = "<="
  pretty _ GT = PP.rangle
  pretty _ GE = ">="

prettyPrintMakefile :: ( Context (Makefile x) ~ Int
                       , Pretty (Makefile x)
                       ) => Makefile x -> TL.Text
prettyPrintMakefile = PT.renderLazy . PP.layoutCompact . pretty 0
