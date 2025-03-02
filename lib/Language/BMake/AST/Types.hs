{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.BMake.AST.Types
  ( Makefile(..)
  , Comment(..)
  , Block(..)
  , Blank(..)
  , Assignment(..)
  , AssignmentOp(..)
  , Value(..)
  , UnstructuredText(..)
  , Rule(..)
  , Dependency(..)
  , DependencyType(..)
  , ShellCmd(..)
  , CommandMode(..)
  , Directive(..)
  , Include(..)
  , IncMode(..)
  , IncLoc(..)
  , Message(..)
  , MessageType(..)
  , Export(..)
  , ExportWay(..)
  , ExportAll(..)
  , UnexportEnv(..)
  , Undef(..)
  , Conditional(..)
  , Else(..)
  , EndIf(..)
  , CondBranch(..)
  , Condition(..)
  , LogicalExpr(..)
  , RelationalOp(..)
  , Expr(..)
  , ForLoop(..)
  , For(..)
  , EndFor(..)
  , Break(..)
  ) where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.MonoTraversable
  ( Element, GrowingAppend, MonoFoldable(..), MonoFunctor(..)
  , MonoPointed(..) )
import Data.String (IsString(..))
import Data.Text (Text)
import Language.BMake.AST.Extension
import Prelude hiding (Ordering(..))


type instance Element (Makefile x) = Block x
newtype Makefile x = Makefile { blocks :: [Block x] }
  deriving newtype ( GrowingAppend, MonoFoldable, MonoFunctor, MonoPointed
                   , Semigroup, Monoid )
-- NOTE: Can't derive MonoTraversable due to
-- https://stackoverflow.com/questions/49776924/newtype-deriving-issequence
deriving instance (Data x, Data (Block x)) => Data (Makefile x)
deriving instance Show (Block x) => Show (Makefile x)
deriving instance Eq   (Block x) => Eq   (Makefile x)

data Comment x = Comment !(XComment x) !Text
deriving instance (Data x, Data (XComment x)) => Data (Comment x)
deriving instance Show (XComment x) => Show (Comment x)
deriving instance Eq   (XComment x) => Eq   (Comment x)
instance XComment x ~ () => IsString (Comment x) where
  fromString = Comment () . fromString

data Block x
  = BBlank      !(Blank x)
  | BAssignment !(Assignment x)
  | BRule       !(Rule x)
  | BDirective  !(Directive x)
deriving instance ( Data x
                  , Data (Blank x)
                  , Data (Assignment x)
                  , Data (Rule x)
                  , Data (Directive x)
                  ) => Data (Block x)
deriving instance ( Show (Blank x)
                  , Show (Assignment x)
                  , Show (Rule x)
                  , Show (Directive x)
                  ) => Show (Block x)
deriving instance ( Eq   (Blank x)
                  , Eq   (Assignment x)
                  , Eq   (Rule x)
                  , Eq   (Directive x)
                  ) => Eq (Block x)

-- |A blank line.
data Blank x =
  Blank
  { bExt     :: !(XBlank x)
  , bComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XBlank x)
                  , Data (Comment x)
                  ) => Data (Blank x)
deriving instance ( Show (XBlank x)
                  , Show (Comment x)
                  ) => Show (Blank x)
deriving instance ( Eq   (XBlank x)
                  , Eq   (Comment x)
                  ) => Eq   (Blank x)

data Assignment x =
  Assignment
  { aExt     :: !(XAssignment x)
  , aVar     :: !(Value x)
  , aOp      :: !AssignmentOp
  , aValues  :: ![Value x]
  , aComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XAssignment x)
                  , Data (Value x)
                  , Data (Value x)
                  , Data (Comment x)
                  ) => Data (Assignment x)
deriving instance ( Show (XAssignment x)
                  , Show (Value x)
                  , Show (Value x)
                  , Show (Comment x)
                  ) => Show (Assignment x)
deriving instance ( Eq   (XAssignment x)
                  , Eq   (Value x)
                  , Eq   (Value x)
                  , Eq   (Comment x)
                  ) => Eq   (Assignment x)

data AssignmentOp
  = Set            -- ^@=@
  | Append         -- ^@+=@
  | SetIfUndefined -- ^@?=@
  | ExpandThenSet  -- ^@:=@
  | ExecThenSet    -- ^@!=@
  deriving (Data, Show, Eq)

data Value x = Value { vExt  :: !(XValue x)
                     , vText :: !Text
                     }
deriving instance (Data x, Data (XValue x)) => Data (Value x)
deriving instance Show (XValue x) => Show (Value x)
deriving instance Eq   (XValue x) => Eq   (Value x)

newtype UnstructuredText = UnstructuredText Text
  deriving (Data, Show, Eq, IsString)

data Rule x
  = Rule
    { rDependency :: !(Dependency x)
    , rCommands   :: ![ShellCmd x]
    }
deriving instance ( Data x
                  , Data (Dependency x)
                  , Data (ShellCmd x)
                  ) => Data (Rule x)
deriving instance ( Show (Dependency x)
                  , Show (ShellCmd x)
                  ) => Show (Rule x)
deriving instance ( Eq   (Dependency x)
                  , Eq   (ShellCmd x)
                  ) => Eq   (Rule x)

data Dependency x =
  Dependency
  { dExt     :: !(XDependency x)
  , dTargets :: !(NonEmpty (Value x))
  , dType    :: !DependencyType
  , dSources :: ![Value x]
  , dComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XDependency x)
                  , Data (Value x)
                  , Data (Comment x)
                  ) => Data (Dependency x)
deriving instance ( Show (XDependency x)
                  , Show (Value x)
                  , Show (Comment x)
                  ) => Show (Dependency x)
deriving instance ( Eq   (XDependency x)
                  , Eq   (Value x)
                  , Eq   (Comment x)
                  ) => Eq   (Dependency x)

data DependencyType
  = IfOlderThan  -- ^@:@
  | Always       -- ^@!@
  | NoAccumulate -- ^@::@
  deriving (Data, Show, Eq)

data ShellCmd x =
  ShellCmd
  { sExt     :: !(XShellCmd x)
  , sModes   :: ![CommandMode]
  , sCommand :: !UnstructuredText
  , sComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XShellCmd x)
                  , Data (Comment x)
                  ) => Data (ShellCmd x)
deriving instance ( Show (XShellCmd x)
                  , Show (Comment x)
                  ) => Show (ShellCmd x)
deriving instance ( Eq   (XShellCmd x)
                  , Eq   (Comment x)
                  ) => Eq   (ShellCmd x)

data CommandMode
  = NoEcho -- ^@\@@
  | Dry    -- ^@+@
  | IgnErr -- ^@-@
  deriving (Data, Show, Eq)

data Directive x
  = DInclude     !(Include x)
  | DMessage     !(Message x)      -- ^@.{error,info,warning} MESSAGE@
  | DExport      !(Export x)       -- ^@.export VARIABLE ...@ and its families
  | DExportAll   !(ExportAll x)    -- ^@.export-all@
  | DUnexportEnv !(UnexportEnv x)  -- ^@.unexport-env@
  | DUndef       !(Undef x)        -- ^@.undef VARIABLE@
  | DConditional !(Conditional x)  -- ^@.if@ and its families
  | DFor         !(ForLoop x)      -- ^@.for@
  | DBreak       !(Break x)        -- ^@.break@
deriving instance ( Data x
                  , Data (Include x)
                  , Data (Message x)
                  , Data (Export x)
                  , Data (ExportAll x)
                  , Data (UnexportEnv x)
                  , Data (Undef x)
                  , Data (Conditional x)
                  , Data (ForLoop x)
                  , Data (Break x)
                  ) => Data (Directive x)
deriving instance ( Show (Include x)
                  , Show (Message x)
                  , Show (Export x)
                  , Show (ExportAll x)
                  , Show (UnexportEnv x)
                  , Show (Undef x)
                  , Show (Conditional x)
                  , Show (ForLoop x)
                  , Show (Break x)
                  ) => Show (Directive x)
deriving instance ( Eq   (Include x)
                  , Eq   (Message x)
                  , Eq   (Export x)
                  , Eq   (UnexportEnv x)
                  , Eq   (ExportAll x)
                  , Eq   (Undef x)
                  , Eq   (Conditional x)
                  , Eq   (ForLoop x)
                  , Eq   (Break x)
                  ) => Eq   (Directive x)

data Include x =
  Include
  { iExt     :: !(XInclude x)
  , iMode    :: !IncMode
  , iLoc     :: !IncLoc
  , iPath    :: !Text
  , iComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XInclude x)
                  , Data (Comment x)
                  ) => Data (Include x)
deriving instance ( Show (XInclude x)
                  , Show (Comment x)
                  ) => Show (Include x)
deriving instance ( Eq   (XInclude x)
                  , Eq   (Comment x)
                  ) => Eq (Include x)

data IncMode
  = Normal -- ^@.include@
  | SMode  -- ^@.sinclude@
  | DMode  -- ^@.dinclude@
  deriving (Data, Show, Eq)

data IncLoc
  = System -- ^@.include <FILE>@
  | User   -- ^@.include "FILE"@
  deriving (Data, Show, Eq)

data Message x =
  Message
  { msgExt     :: !(XMessage x)
  , msgType    :: !MessageType
  , msgText    :: !UnstructuredText
  , msgComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XMessage x)
                  , Data (Comment x)
                  ) => Data (Message x)
deriving instance ( Show (XMessage x)
                  , Show (Comment x)
                  ) => Show (Message x)
deriving instance ( Eq   (XMessage x)
                  , Eq   (Comment x)
                  ) => Eq   (Message x)

data MessageType = Info | Warning | Error
  deriving (Data, Show, Eq)

data Export x =
  Export
  { expExt     :: !(XExport x)
  , expWay     :: !ExportWay
  , expVars    :: ![Value x]
  , expComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XExport x)
                  , Data (Value x)
                  , Data (Comment x)
                  ) => Data (Export x)
deriving instance ( Show (XExport x)
                  , Show (Value x)
                  , Show (Comment x)
                  ) => Show (Export x)
deriving instance ( Eq   (XExport x)
                  , Eq   (Value x)
                  , Eq   (Comment x)
                  ) => Eq (Export x)

data ExportWay =
    Exp    -- ^@.export@
  | ExpEnv -- ^@.export-env@
  | ExpLit -- ^@.export-literal@
  | Unexp  -- ^@.unexport@
  deriving (Data, Show, Eq)

data ExportAll x = ExportAll !(XExportAll x) !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XExportAll x)
                  , Data (Comment x)
                  ) => Data (ExportAll x)
deriving instance ( Show (XExportAll x)
                  , Show (Comment x)
                  ) => Show (ExportAll x)
deriving instance ( Eq   (XExportAll x)
                  , Eq   (Comment x)
                  ) => Eq   (ExportAll x)

data UnexportEnv x = UnexportEnv !(XUnexportEnv x) !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XUnexportEnv x)
                  , Data (Comment x)
                  ) => Data (UnexportEnv x)
deriving instance ( Show (XUnexportEnv x)
                  , Show (Comment x)
                  ) => Show (UnexportEnv x)
deriving instance ( Eq   (XUnexportEnv x)
                  , Eq   (Comment x)
                  ) => Eq   (UnexportEnv x)

data Undef x = Undef !(XUndef x) ![Value x] !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XUndef x)
                  , Data (Value x)
                  , Data (Comment x)
                  ) => Data (Undef x)
deriving instance ( Show (XUndef x)
                  , Show (Value x)
                  , Show (Comment x)
                  ) => Show (Undef x)
deriving instance ( Eq (XUndef x)
                  , Eq (Value x)
                  , Eq (Comment x)
                  ) => Eq (Undef x)

data Conditional x
  = Conditional
    { condExt      :: !(XConditional x)
    , condBranches :: !(NonEmpty (CondBranch x))
    , condElse     :: !(Maybe (Else x))
    , condEnd      :: !(EndIf x)
    }
deriving instance ( Data x
                  , Data (XConditional x)
                  , Data (CondBranch x)
                  , Data (Else x)
                  , Data (EndIf x)
                  ) => Data (Conditional x)
deriving instance ( Show (XConditional x)
                  , Show (CondBranch x)
                  , Show (Else x)
                  , Show (EndIf x)
                  ) => Show (Conditional x)
deriving instance ( Eq   (XConditional x)
                  , Eq   (CondBranch x)
                  , Eq   (Else x)
                  , Eq   (EndIf x)
                  ) => Eq   (Conditional x)

instance Semigroup (Conditional x) where
  ca <> cb
    = case condElse ca of
        Nothing ->
          -- ca has no .else block, which means we can merge ca and cb
          -- without nesting them.
          ca { condBranches = condBranches ca <> condBranches cb
             , condElse     = condElse cb
             }

        Just (Else ext com ea) ->
          -- ca has an .else block, which means we must put cb inside the
          -- block.
          ca { condElse = Just (Else ext com $ ea <> Makefile [BDirective $ DConditional cb]) }

-- |@.else@
data Else x = Else !(XElse x) !(Maybe (Comment x)) !(Makefile x)
deriving instance ( Data x
                  , Data (XElse x)
                  , Data (Makefile x)
                  , Data (Comment x)
                  ) => Data (Else x)
deriving instance ( Show (XElse x)
                  , Show (Makefile x)
                  , Show (Comment x)
                  ) => Show (Else x)
deriving instance ( Eq   (XElse x)
                  , Eq   (Makefile x)
                  , Eq   (Comment x)
                  ) => Eq   (Else x)

-- |@.endif@
data EndIf x = EndIf !(XEndIf x) !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XEndIf x)
                  , Data (Comment x)
                  ) => Data (EndIf x)
deriving instance ( Show (XEndIf x)
                  , Show (Comment x)
                  ) => Show (EndIf x)
deriving instance ( Eq   (XEndIf x)
                  , Eq   (Comment x)
                  ) => Eq   (EndIf x)

data CondBranch x = CondBranch !(Condition x) !(Makefile x)
deriving instance ( Data x
                  , Data (Condition x)
                  , Data (Makefile x)
                  ) => Data (CondBranch x)
deriving instance ( Show (Condition x)
                  , Show (Makefile x)
                  ) => Show (CondBranch x)
deriving instance ( Eq   (Condition x)
                  , Eq   (Makefile x)
                  ) => Eq   (CondBranch x)

-- |For 'Ifdef' and 'Ifmake', boolean 'True' means a positive condition
-- (@.ifdef@/@.ifmake@) and 'False' means a negative condition
-- (@.ifndef@/@.ifnmake@).
data Condition x
  = If      !(XIf     x)       !(LogicalExpr x (Expr  x)) !(Maybe (Comment x)) -- ^@.if@
  | Ifdef   !(XIfdef  x) !Bool !(LogicalExpr x (Value x)) !(Maybe (Comment x)) -- ^@.ifdef@/@.ifndef@
  | Ifmake  !(XIfmake x) !Bool !(LogicalExpr x (Value x)) !(Maybe (Comment x)) -- ^@.ifmake@/@.ifnmake@
deriving instance ( Data x
                  , Data (XIf x)
                  , Data (XIfdef x)
                  , Data (XIfmake x)
                  , Data (LogicalExpr x (Expr x))
                  , Data (LogicalExpr x (Value x))
                  , Data (Comment x)
                  ) => Data (Condition x)
deriving instance ( Show (XIf x)
                  , Show (XIfdef x)
                  , Show (XIfmake x)
                  , Show (LogicalExpr x (Expr x))
                  , Show (LogicalExpr x (Value x))
                  , Show (Comment x)
                  ) => Show (Condition x)
deriving instance ( Eq   (XIf x)
                  , Eq   (XIfdef x)
                  , Eq   (XIfmake x)
                  , Eq   (LogicalExpr x (Expr x))
                  , Eq   (LogicalExpr x (Value x))
                  , Eq   (Comment x)
                  ) => Eq   (Condition x)

data LogicalExpr x a
  = Not  !(XNot  x) !(LogicalExpr x a)            -- ^@!@, which is of higher precedence than @&&@.
  | And  !(XAnd  x) !(NonEmpty (LogicalExpr x a)) -- ^@&&@, which is of higher precedence than @||@.
  | Or   !(XOr   x) !(NonEmpty (LogicalExpr x a)) -- ^@||@
  | Expr !(XExpr x) !a
  | ExpLE !(XExpLE x a)                           -- ^Extended constructor
deriving instance ( Data x
                  , Data a
                  , Data (XNot   x)
                  , Data (XOr    x)
                  , Data (XAnd   x)
                  , Data (XExpr  x)
                  , Data (XExpLE x a)
                  ) => Data (LogicalExpr x a)
deriving instance ( Show a
                  , Show (XNot   x)
                  , Show (XOr    x)
                  , Show (XAnd   x)
                  , Show (XExpr  x)
                  , Show (XExpLE x a)
                  ) => Show (LogicalExpr x a)
deriving instance ( Eq   a
                  , Eq   (XNot   x)
                  , Eq   (XOr    x)
                  , Eq   (XAnd   x)
                  , Eq   (XExpr  x)
                  , Eq   (XExpLE x a)
                  ) => Eq   (LogicalExpr x a)

data RelationalOp
  = EQ -- ^@==@
  | NE -- ^@!=@
  | LT -- ^@<@
  | LE -- ^@<=@
  | GT -- ^@>@
  | GE -- ^@>=@
  deriving (Data, Show, Eq)

data Expr x
  = EDefined  !Text -- ^@defined(VARIABLE)@
  | EMake     !Text -- ^@make(TARGET)@
  | EEmpty    !Text -- ^@empty(VARIABLE)@
  | EExists   !Text -- ^@exists(FILE)@
  | ETarget   !Text -- ^@target(TARGET)@
  | ECommands !Text -- ^@commands(TARGET)@
  | ECompare
    { eCmpExt :: !(XECompare x)
    , eCmpLHS :: !(Value x)
    , eCmpRHS :: !(Maybe (RelationalOp, Value x))
    }
deriving instance ( Data x
                  , Data (Value x)
                  , Data (XECompare x)
                  , Data (Value x)
                  ) => Data (Expr x)
deriving instance ( Show (Value x)
                  , Show (XECompare x)
                  , Show (Value x)
                  ) => Show (Expr x)
deriving instance ( Eq   (Value x)
                  , Eq   (XECompare x)
                  , Eq   (Value x)
                  ) => Eq   (Expr x)

data ForLoop x = ForLoop !(For x) !(Makefile x) !(EndFor x)
deriving instance ( Data x
                  , Data (For x)
                  , Data (Makefile x)
                  , Data (EndFor x)
                  ) => Data (ForLoop x)
deriving instance ( Show (For x)
                  , Show (Makefile x)
                  , Show (EndFor x)
                  ) => Show (ForLoop x)
deriving instance ( Eq   (For x)
                  , Eq   (Makefile x)
                  , Eq   (EndFor x)
                  ) => Eq   (ForLoop x)

data For x =
  For
  { forExt     :: !(XFor x)
  , forVars    :: !(NonEmpty (Value x))
  , forExpr    :: !UnstructuredText
  , forComment :: !(Maybe (Comment x))
  }
deriving instance ( Data x
                  , Data (XFor x)
                  , Data (Value x)
                  , Data (Comment x)
                  ) => Data (For x)
deriving instance ( Show (XFor x)
                  , Show (Value x)
                  , Show (Comment x)
                  ) => Show (For x)
deriving instance ( Eq   (XFor x)
                  , Eq   (Value x)
                  , Eq   (Comment x)
                  ) => Eq   (For x)

data EndFor x = EndFor !(XEndFor x) !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XEndFor x)
                  , Data (Comment x)
                  ) => Data (EndFor x)
deriving instance ( Show (XEndFor x)
                  , Show (Comment x)
                  ) => Show (EndFor x)
deriving instance ( Eq   (XEndFor x)
                  , Eq   (Comment x)
                  ) => Eq   (EndFor x)

data Break x = Break !(XBreak x) !(Maybe (Comment x))
deriving instance ( Data x
                  , Data (XBreak x)
                  , Data (Comment x)
                  ) => Data (Break x)
deriving instance ( Show (XBreak x)
                  , Show (Comment x)
                  ) => Show (Break x)
deriving instance ( Eq   (XBreak x)
                  , Eq   (Comment x)
                  ) => Eq   (Break x)
