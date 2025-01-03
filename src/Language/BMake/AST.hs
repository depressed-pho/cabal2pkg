{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Language.BMake.AST
  ( -- * Types
    Makefile(..)
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
  , IncMode(..)
  , IncLoc(..)
  , Conditional(..)
  , CondBranch(..)
  , Condition(..)
  , LogicalExpr(..)
  , RelationalOp(..)
  , Expr(..)
  , ForLoop(..)

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

import Data.Data (Data)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isNothing)
import Data.MonoTraversable
  ( Element, GrowingAppend, MonoFoldable(..), MonoFunctor(..)
  , MonoPointed(..) )
import Data.String (IsString(..))
import Data.Semigroup (sconcat, stimesMonoid)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Prelude hiding (Ordering(..))
import Prettyprinter ((<+>), Doc)
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

tabWidth :: Integral n => n
tabWidth = 8

tab :: Doc ann
tab = PP.pretty '\t'

dotSpace :: Int -> Doc ann
dotSpace depth = PP.dot <> stimesMonoid (depth * 2) PP.space

type instance Element Makefile = Block
newtype Makefile = Makefile { blocks :: [Block] }
  deriving stock   (Data, Show, Eq)
  deriving newtype ( GrowingAppend, MonoFoldable, MonoFunctor, MonoPointed
                   , Semigroup, Monoid )
-- NOTE: Can't derive MonoTraversable due to
-- https://stackoverflow.com/questions/49776924/newtype-deriving-issequence

instance Pretty Makefile where
  -- |The current depth of nested directives.
  type Context Makefile = Int
  pretty depth = go mempty . blocks
    where
      -- Align consecutive assignments.
      go :: Seq Assignment -> [Block] -> Doc ann
      go as []     = pprAssignments as
      go as (b:bs) =
        case b of
          BBlank      x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BAssignment a -> go (as |> a) bs
          BRule       x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BDirective  x -> pprAssignments as <> pretty depth x <> go mempty bs

newtype Comment = Comment Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

instance Pretty Comment where
  pretty _ (Comment c)
    = PP.pretty '#' <+> PP.pretty c

-- | Optionally render a comment at the end of a supposedly non-empty
-- line. There will be a TAB character inserted right before @#@.
pprOptionalComment :: Maybe Comment -> Doc ann
pprOptionalComment = foldMap ((tab <>) . pretty ())

data Block
  = BBlank      !Blank
  | BAssignment !Assignment
  | BRule       !Rule
  | BDirective  !Directive
  deriving (Data, Show, Eq)

newtype Variable = Variable Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

instance Pretty Variable where
  pretty _ (Variable name)
    = PP.pretty name

newtype Target = Target Text
  deriving stock (Data, Show, Eq)
  deriving newtype IsString

instance Pretty Target where
  pretty _ (Target path)
    = PP.pretty path

-- |A blank line.
newtype Blank = Blank { bComment :: Maybe Comment }
  deriving stock (Data, Show, Eq)

instance Pretty Blank where
  pretty _ (Blank c) =
    foldMap (pretty ()) c <> PP.line

data Assignment
  = Assignment
    { aVar     :: !Variable
    , aType    :: !AssignmentType
    , aTokens  :: ![Text]
    , aComment :: !(Maybe Comment)
    }
  deriving (Data, Show, Eq)

instance Pretty Assignment where
  -- |The column number to align tokens.
  type Context Assignment = Int
  pretty col (Assignment {..})
    = mconcat [ pretty () aVar <> pretty () aType
              , if hasTokens
                then mconcat
                     [ tabs
                     , pretty () (escapeToken <$> aTokens)
                     , pprOptionalComment aComment
                     ]
                else foldMap (pretty ()) aComment
              , PP.line
              ]
    where
      hasTokens :: Bool
      hasTokens = (not . all T.null) aTokens

      tabs :: Doc ann
      tabs = PP.column $ \cur -> stimesMonoid (nTabs cur) tab

      nTabs :: Int -> Int
      nTabs cur = (max 0 (col - cur - 1) `div` tabWidth) + 1

data AssignmentType
  = Set            -- ^@=@
  | Append         -- ^@+=@
  | SetIfUndefined -- ^@?=@
  | ExpandThenSet  -- ^@:=@
  | ExecThenSet    -- ^@!=@
  deriving (Data, Show, Eq)

instance Pretty AssignmentType where
  pretty _ Set            = PP.equals
  pretty _ Append         = "+="
  pretty _ SetIfUndefined = "?="
  pretty _ ExpandThenSet  = ":="
  pretty _ ExecThenSet    = "!="

data Rule
  = Rule
    { rDependency :: !Dependency
    , rComment    :: !(Maybe Comment)
    , rCommands   :: ![ShellCmd]
    }
  deriving (Data, Show, Eq)

instance Pretty Rule where
  pretty _ (Rule {..})
    = mconcat [ pretty () rDependency
              , pprOptionalComment rComment
              , PP.line
              , mconcat $ pretty () <$> rCommands
              ]

data Dependency
  = Dependency
    { dTargets :: ![Target]
    , dType    :: !DependencyType
    , dSources :: ![Text]
    }
  deriving (Data, Show, Eq)

instance Pretty Dependency where
  pretty _ (Dependency {..})
    = mconcat [ pretty () dTargets
              , pretty () dType
              , PP.space
              , pretty () dSources
              ]

data DependencyType
  = IfOlderThan  -- ^@:@
  | Always       -- ^@!@
  | NoAccumulate -- ^@::@
  deriving (Data, Show, Eq)

instance Pretty DependencyType where
  pretty _ IfOlderThan  = PP.colon
  pretty _ Always       = PP.pretty '!'
  pretty _ NoAccumulate = "::"

data ShellCmd = ShellCmd ![CommandMode] !Text
  deriving (Data, Show, Eq)

instance Pretty ShellCmd where
  pretty _ (ShellCmd modes cmd)
    = mconcat [ tab
              , mconcat $ pretty () <$> modes
              , PP.pretty cmd
              ]

data CommandMode
  = NoEcho -- ^@\@@
  | Dry    -- ^@+@
  | IgnErr -- ^@-@
  deriving (Data, Show, Eq)

instance Pretty CommandMode where
  pretty _ NoEcho = PP.pretty '@'
  pretty _ Dry    = PP.pretty '+'
  pretty _ IgnErr = PP.pretty '-'

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

instance Pretty Directive where
  -- |The current depth of nested directives.
  type Context Directive = Int
  pretty depth dir = go <> PP.line
    where
      go :: Doc ann
      go = case dir of
             DInclude     inc  -> pretty depth inc
             DError       msg  -> dot <> "error"          <+> PP.pretty msg
             DExport      vars -> dot <> "export"         <+> pretty () vars
             DExportEnv   vars -> dot <> "export-env"     <+> pretty () vars
             DExportLit   vars -> dot <> "export-literal" <+> pretty () vars
             DInfo        msg  -> dot <> "info"           <+> PP.pretty msg
             DUndef       var  -> dot <> "undef"          <+> pretty () var
             DUnexport    vars -> dot <> "unexport"       <+> pretty () vars
             DUnexportEnv vars -> dot <> "unexport-env"   <+> pretty () vars
             DWarning     msg  -> dot <> "warning"        <+> PP.pretty msg
             DConditional cond -> pretty depth cond
             DFor         for  -> pretty depth for

      dot :: Doc ann
      dot = dotSpace depth

data Include = Include !IncMode !IncLoc !Text
  deriving (Data, Show, Eq)

instance Pretty Include where
  -- |The current depth of nested directives.
  type Context Include = Int
  pretty depth (Include mode loc file) =
    mconcat [ dotSpace depth
            , pretty () mode
            , PP.space
            , case loc of
                System -> PP.angles  . PP.pretty $ file
                User   -> PP.dquotes . PP.pretty $ file
            ]

data IncMode
  = Normal -- ^@.include@
  | SMode  -- ^@.sinclude@
  | DMode  -- ^@.dinclude@
  deriving (Data, Show, Eq)

instance Pretty IncMode where
  pretty _ Normal = "include"
  pretty _ SMode  = "sinclude"
  pretty _ DMode  = "dinclude"

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

instance Pretty Conditional where
  -- |The current depth of nested directives.
  type Context Conditional = Int
  pretty depth (Conditional {..}) =
    mconcat [ pprBranches branches
            , pprElse else_
            , dotSpace depth
            , "endif"
            , pprOptionalComment endComment
            ]
    where
      contentDepth :: Int
      contentDepth
        | indent    = depth + 1
        | otherwise = depth

      pprBranches :: NonEmpty CondBranch -> Doc ann
      pprBranches (br :| brs)
        = pretty (True, contentDepth) br <>
          mconcat (pretty (False, contentDepth) <$> brs)

      pprElse :: Maybe Makefile -> Doc ann
      pprElse Nothing  = mempty
      pprElse (Just m) = PP.vsep [ dotSpace depth <> "else"
                                 , pretty contentDepth m
                                 ]

data CondBranch = CondBranch !Condition !Makefile
  deriving (Data, Show, Eq)

instance Pretty CondBranch where
  -- |The 'Bool' value indicates whether the branch is the first one,
  -- e.g. @.if@ as opposed to @.elif@. The 'Int' value represents the
  -- desired, not current, depth of nested directives.
  type Context CondBranch = (Bool, Int)
  pretty ctx@(_, depth) (CondBranch cond m) =
    PP.vsep [ pretty ctx cond
            , pretty depth m
            ]

data Condition
  = If      !(LogicalExpr Expr    ) -- ^@.if@
  | Ifdef   !(LogicalExpr Variable) -- ^@.ifdef@
  | Ifndef  !(LogicalExpr Variable) -- ^@.ifndef@
  | Ifmake  !(LogicalExpr Target  ) -- ^@.ifmake@
  | Ifnmake !(LogicalExpr Target  ) -- ^@.ifnmake@
  deriving (Data, Show, Eq)

instance Pretty Condition where
  type Context Condition = (Bool, Int)
  pretty (isFirst, depth) cond
    = dotSpace depth <> go cond
    where
      go :: Condition -> Doc ann
      go (If      expr) = switch "if"      <+> pretty (False, ()) expr
      go (Ifdef   expr) = switch "ifdef"   <+> pretty (False, ()) expr
      go (Ifndef  expr) = switch "ifndef"  <+> pretty (False, ()) expr
      go (Ifmake  expr) = switch "ifmake"  <+> pretty (False, ()) expr
      go (Ifnmake expr) = switch "ifnmake" <+> pretty (False, ()) expr

      switch :: Doc ann -> Doc ann
      switch b
        | isFirst   = b
        | otherwise = "el" <> b

data LogicalExpr a
  = Not  !(LogicalExpr a)            -- ^@!@
  | Or   !(NonEmpty (LogicalExpr a)) -- ^@||@
  | And  !(NonEmpty (LogicalExpr a)) -- ^@&&@
  | Expr !a
  deriving (Data, Show, Eq)

instance Pretty a => Pretty (LogicalExpr a) where
  -- |Whether the expression is a nested one.
  type Context (LogicalExpr a) = (Bool, Context a)
  pretty (isNested, ctx) le =
    case le of
      Not e  -> maybeParens isNested $ PP.pretty '!' <> pretty (True, ctx) e
      Or  es -> maybeParens isNested . sconcat . NE.intersperse " || " $ pretty (True, ctx) <$> es
      And es -> maybeParens isNested . sconcat . NE.intersperse " && " $ pretty (True, ctx) <$> es
      Expr e -> pretty ctx e
    where
      maybeParens :: Bool -> Doc ann -> Doc ann
      maybeParens True  = PP.parens
      maybeParens False = id

data RelationalOp
  = EQ -- ^@==@
  | NE -- ^@!=@
  | LT -- ^@<@
  | LE -- ^@<=@
  | GT -- ^@>@
  | GE -- ^@>=@
  deriving (Data, Show, Eq)

instance Pretty RelationalOp where
  pretty _ EQ = "=="
  pretty _ NE = "!="
  pretty _ LT = PP.langle
  pretty _ LE = "<="
  pretty _ GT = PP.rangle
  pretty _ GE = ">="

data Expr
  = EDefined  !Variable -- ^@defined(VARIABLE)@
  | EMake     !Target   -- ^@make(TARGET)@
  | EEmpty    !Text     -- ^@empty(VARIABLE)@
  | EExists   !Text     -- ^@exists(FILE)@
  | ETarget   !Target   -- ^@target(TARGET)@
  | ECommands !Target   -- ^@commands(TARGET)@
  | ECompare  !Text !(Maybe (RelationalOp, Text))
  deriving (Data, Show, Eq)

instance Pretty Expr where
  pretty _ (EDefined  var ) = "defined"  <> PP.parens (pretty () var )
  pretty _ (EMake     tgt ) = "make"     <> PP.parens (pretty () tgt )
  pretty _ (EEmpty    var ) = "empty"    <> PP.parens (pretty () var )
  pretty _ (EExists   file) = "exists"   <> PP.parens (pretty () file)
  pretty _ (ETarget   tgt ) = "target"   <> PP.parens (pretty () tgt )
  pretty _ (ECommands tgt ) = "commands" <> PP.parens (pretty () tgt )
  pretty _ (ECompare lhs mRhs)
    = case mRhs of
        Nothing        -> PP.pretty lhs
        Just (op, rhs) -> PP.hsep [ PP.pretty lhs
                                  , pretty () op
                                  , PP.pretty rhs
                                  ]

data ForLoop = ForLoop ![Variable] !Text !Makefile
  deriving (Data, Show, Eq)

instance Pretty ForLoop where
  -- |The current depth of nested directives.
  type Context ForLoop = Int
  pretty depth (ForLoop vars expr body) =
    PP.vsep [ dotSpace depth <> "for" <+> pretty () vars <+> "in" <+> pretty () expr
            , pretty (depth + 1) body <> dotSpace depth <> "endfor"
            ]

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

prettyPrintAST :: Makefile -> TL.Text
prettyPrintAST = PT.renderLazy . PP.layoutCompact . pretty 0

pprAssignments :: Foldable t => t Assignment -> Doc ann
pprAssignments as
  | pprNormally = foldr (\a -> (pretty alignment a <>)) mempty as
  | otherwise   = foldr (\a -> (pretty () (FoldedAssignment a) <>)) mempty as
  where
    alignment :: Int
    alignment = foldl' maxAlign 0 as

    maxAlign :: Int -> Assignment -> Int
    maxAlign n (Assignment {..}) =
      -- This is essentially a two-pass operation, so we cannot implement
      -- it in terms of things like PP.width
      let pre     = pretty () aVar <> pretty () aType
          render  = PT.renderLazy . PP.layoutCompact
          len     = fromIntegral . TL.length . render $ pre
          aligned = ((len `div` tabWidth) + 1) * tabWidth
      in
        max n aligned

    pprNormally :: Bool
    pprNormally = alignment < 32 || length as > 1

-- |A special case for singular assignments (i.e. ones that don't have
-- preceding or following assignments) whose variable name is very
-- long. Render them like this:
--
-- > SOME_VERY_LONG_VARIABLE+=	\
-- > 	foo	\
-- > 	bar
--
-- And not like this:
--
-- > SOME_VERY_LONG_VARIABLE+=	foo bar
--
newtype FoldedAssignment = FoldedAssignment Assignment

instance Pretty FoldedAssignment where
  pretty _ (FoldedAssignment (Assignment {..}))
    = mconcat [ pretty () aVar
              , pretty () aType
              , if null aTokens && isNothing aComment
                then PP.line
                else mconcat [ tab
                             , PP.backslash
                             , PP.line
                             , go (escapeToken <$> aTokens)
                             , pprOptionalComment aComment
                             , PP.line
                             ]
              ]
    where
      go :: [Text] -> Doc ann
      go []     = error "impossible"
      go [x]    = tab <> PP.pretty x
      go (x:xs) = mconcat [ tab
                          , PP.pretty x
                          , tab
                          , PP.backslash
                          , PP.line
                          , go xs
                          ]

-- |Escape @#@ in a variable assignment.
escapeToken :: Text -> Text
escapeToken = T.replace "#"  "\\#"
            . T.replace "\\" "\\\\"
