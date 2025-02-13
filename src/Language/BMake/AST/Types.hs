{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
module Language.BMake.AST.Types
  ( Makefile(..)
  , Comment(..)
  , Block(..)
  , Variable(..)
  , Target(..)
  , Blank(..)
  , Assignment(..)
  , AssignmentType(..)
  , Rule(..)
  , Dependency(..)
  , DependencyType(..)
  , ShellCmd(..)
  , CommandMode(..)
  , Directive(..)
  , Include(..)
  , IncMode(..)
  , IncLoc(..)
  , Conditional(..)
  , CondBranch(..)
  , Condition(..)
  , LogicalExpr(..)
  , RelationalOp(..)
  , Expr(..)
  , ForLoop(..)
  ) where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.MonoTraversable
  ( Element, GrowingAppend, MonoFoldable(..), MonoFunctor(..)
  , MonoPointed(..) )
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (Ordering(..))


type instance Element Makefile = Block
newtype Makefile = Makefile { blocks :: [Block] }
  deriving stock   (Data, Show, Eq)
  deriving newtype ( GrowingAppend, MonoFoldable, MonoFunctor, MonoPointed
                   , Semigroup, Monoid )
-- NOTE: Can't derive MonoTraversable due to
-- https://stackoverflow.com/questions/49776924/newtype-deriving-issequence

newtype Comment = Comment Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

data Block
  = BBlank      !Blank
  | BAssignment !Assignment
  | BRule       !Rule
  | BDirective  !Directive
  deriving (Data, Show, Eq)

newtype Variable = Variable Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

newtype Target = Target Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

-- |A blank line.
newtype Blank = Blank { bComment :: Maybe Comment }
  deriving stock (Data, Show, Eq)

data Assignment
  = Assignment
    { aVar     :: !Variable
    , aType    :: !AssignmentType
    , aTokens  :: ![Text]
    , aComment :: !(Maybe Comment)
    }
  deriving (Data, Show, Eq)

data AssignmentType
  = Set            -- ^@=@
  | Append         -- ^@+=@
  | SetIfUndefined -- ^@?=@
  | ExpandThenSet  -- ^@:=@
  | ExecThenSet    -- ^@!=@
  deriving (Data, Show, Eq)

data Rule
  = Rule
    { rDependency :: !Dependency
    , rComment    :: !(Maybe Comment)
    , rCommands   :: ![ShellCmd]
    }
  deriving (Data, Show, Eq)

data Dependency
  = Dependency
    { dTargets :: ![Target]
    , dType    :: !DependencyType
    , dSources :: ![Text]
    }
  deriving (Data, Show, Eq)

data DependencyType
  = IfOlderThan  -- ^@:@
  | Always       -- ^@!@
  | NoAccumulate -- ^@::@
  deriving (Data, Show, Eq)

data ShellCmd = ShellCmd ![CommandMode] !Text
  deriving (Data, Show, Eq)

data CommandMode
  = NoEcho -- ^@\@@
  | Dry    -- ^@+@
  | IgnErr -- ^@-@
  deriving (Data, Show, Eq)

data Directive
  = DInclude     !Include
  | DError       !Text             -- ^@.error MESSAGE@
  | DExport      ![Variable]       -- ^@.export VARIABLE ...@
  | DExportEnv   ![Variable]       -- ^@.export-env VARIABLE ...@
  | DExportLit   ![Variable]       -- ^@.export-literal VARIABLE ...@
  | DInfo        !Text             -- ^@.info MESSAGE@
  | DUndef       !Variable         -- ^@.undef VARIABLE@
  | DUnexport    ![Variable]       -- ^@.unexport VARIABLE ...@
  | DUnexportEnv ![Variable]       -- ^@.unexport-env VARIABLE ...@
  | DWarning     !Text             -- ^@.warning MESSAGE@
  | DConditional !Conditional      -- ^@.if@ and its families
  | DFor         !ForLoop          -- ^@.for@
  deriving (Data, Show, Eq)

data Include = Include !IncMode !IncLoc !Text
  deriving (Data, Show, Eq)

data IncMode
  = Normal -- ^@.include@
  | SMode  -- ^@.sinclude@
  | DMode  -- ^@.dinclude@
  deriving (Data, Show, Eq)

data IncLoc
  = System -- ^@.include <FILE>@
  | User   -- ^@.include "FILE"@
  deriving (Data, Show, Eq)

data Conditional
  = Conditional
    { branches   :: !(NonEmpty CondBranch)
    , else_      :: !(Maybe Makefile) -- ^@.else@
    , endComment :: !(Maybe Comment)
    -- |Whether to indent the contents of this conditional. This should
    -- usually be 'True'.
    , indent     :: !Bool
    }
  deriving (Data, Show, Eq)

instance Semigroup Conditional where
  ca <> cb
    = case else_ ca of
        Nothing ->
          -- ca has no .else block, which means we can merge ca and cb
          -- without nesting them.
          ca { branches = branches ca <> branches cb
             , else_    = else_ cb
             }

        Just ea ->
          -- ca has a .else block, which means we must put cb inside the
          -- block.
          ca { else_ = Just (ea <> Makefile [BDirective (DConditional cb)]) }

data CondBranch = CondBranch !Condition !Makefile
  deriving (Data, Show, Eq)

data Condition
  = If      !(LogicalExpr Expr    ) -- ^@.if@
  | Ifdef   !(LogicalExpr Variable) -- ^@.ifdef@
  | Ifndef  !(LogicalExpr Variable) -- ^@.ifndef@
  | Ifmake  !(LogicalExpr Target  ) -- ^@.ifmake@
  | Ifnmake !(LogicalExpr Target  ) -- ^@.ifnmake@
  deriving (Data, Show, Eq)

data LogicalExpr a
  = Not  !(LogicalExpr a)            -- ^@!@
  | Or   !(NonEmpty (LogicalExpr a)) -- ^@||@
  | And  !(NonEmpty (LogicalExpr a)) -- ^@&&@
  | Expr !a
  deriving (Data, Show, Eq)

data RelationalOp
  = EQ -- ^@==@
  | NE -- ^@!=@
  | LT -- ^@<@
  | LE -- ^@<=@
  | GT -- ^@>@
  | GE -- ^@>=@
  deriving (Data, Show, Eq)

data Expr
  = EDefined  !Variable -- ^@defined(VARIABLE)@
  | EMake     !Target   -- ^@make(TARGET)@
  | EEmpty    !Text     -- ^@empty(VARIABLE)@
  | EExists   !Text     -- ^@exists(FILE)@
  | ETarget   !Target   -- ^@target(TARGET)@
  | ECommands !Target   -- ^@commands(TARGET)@
  | ECompare  !Text !(Maybe (RelationalOp, Text))
  deriving (Data, Show, Eq)

data ForLoop = ForLoop ![Variable] !Text !Makefile
  deriving (Data, Show, Eq)
