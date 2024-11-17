{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
  , unindent
  , (?==)

    -- * Pretty-printing
  , prettyPrintAST
  ) where

import Data.Data (Data)
import Data.Foldable (foldl')
import Data.List (intersperse)
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
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B
import Prelude hiding (Ordering(..))


class Pretty a where
  type Context a
  type instance Context a = ()
  pretty  :: Context a -> a -> Builder

instance Pretty a => Pretty [a] where
  type Context [a] = Context a
  pretty ctx = mconcat . intersperse space . (pretty ctx <$>)

instance Pretty Text where
  pretty _ = B.fromText

tabWidth :: Integral n => n
tabWidth = 8

backslash :: Builder
backslash = B.singleton '\\'

space :: Builder
space = B.singleton ' '

newline :: Builder
newline = B.singleton '\n'

tab :: Builder
tab = B.singleton '\t'

dotSpace :: Int -> Builder
dotSpace depth = B.singleton '.' <> stimesMonoid (depth * 2) space

parens :: Builder -> Builder
parens b = B.singleton '(' <> b <> B.singleton ')'

maybeParens :: Bool -> Builder -> Builder
maybeParens True  = parens
maybeParens False = id

newtype Makefile = Makefile { blocks :: [Block] }
  deriving (Data, Show, Eq, Semigroup, Monoid)

type instance Element Makefile = Block
deriving instance GrowingAppend Makefile
deriving instance MonoFoldable Makefile
deriving instance MonoFunctor Makefile
deriving instance MonoPointed Makefile
-- NOTE: Can't derive MonoTraversable due to
-- https://stackoverflow.com/questions/49776924/newtype-deriving-issequence

instance Pretty Makefile where
  -- |The current depth of nested directives.
  type Context Makefile = Int
  pretty depth = go mempty . blocks
    where
      -- Align consecutive assignments.
      go :: Seq Assignment -> [Block] -> Builder
      go as []     = pprAssignments as
      go as (b:bs) =
        case b of
          BBlank      x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BAssignment a -> go (as |> a) bs
          BRule       x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BDirective  x -> pprAssignments as <> pretty depth x <> go mempty bs
          BUnindent   x -> pprAssignments as <>
                           pretty (depth-1) (opoint x :: Makefile) <>
                           go mempty bs

newtype Comment = Comment Text
  deriving (Data, Show, Eq, IsString)

instance Pretty Comment where
  pretty _ (Comment c)
    = "# " <> B.fromText c

pprOptionalComment :: Maybe Comment -> Builder
pprOptionalComment = foldMap ((space <>) . pretty ())

data Block
  = BBlank      !Blank
  | BAssignment !Assignment
  | BRule       !Rule
  | BDirective  !Directive
  | BUnindent   !Block
  deriving (Data, Show, Eq)

newtype Variable = Variable Text
  deriving (Data, Show, Eq, IsString)

instance Pretty Variable where
  pretty _ (Variable name)
    = B.fromText name

newtype Target = Target Text
  deriving (Data, Show, Eq, IsString)

instance Pretty Target where
  pretty _ (Target path)
    = B.fromText path

-- |A blank line.
newtype Blank = Blank { bComment :: Maybe Comment }
  deriving (Data, Show, Eq)

instance Pretty Blank where
  pretty _ (Blank c)
    = foldMap (pretty ()) c <> newline

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
    = mconcat [ pre
              , if hasTokens
                then mconcat
                     [ stimesMonoid nTabs tab
                     , pretty () aTokens
                     , pprOptionalComment aComment
                     ]
                else foldMap (pretty ()) aComment
              , newline
              ]
    where
      hasTokens :: Bool
      hasTokens = any (not . T.null) aTokens

      pre :: Builder
      pre = pretty () aVar <> pretty () aType

      len :: Int
      len = fromIntegral . TL.length . B.toLazyText $ pre

      nTabs :: Int
      nTabs = (max 0 (col - len - 1) `div` tabWidth) + 1

data AssignmentType
  = Set            -- ^@=@
  | Append         -- ^@+=@
  | SetIfUndefined -- ^@?=@
  | ExpandThenSet  -- ^@:=@
  | ExecThenSet    -- ^@!=@
  deriving (Data, Show, Eq)

instance Pretty AssignmentType where
  pretty _ Set            = B.singleton '='
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
              , newline
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
              , space
              , pretty () dSources
              ]

data DependencyType
  = IfOlderThan  -- ^@:@
  | Always       -- ^@!@
  | NoAccumulate -- ^@::@
  deriving (Data, Show, Eq)

instance Pretty DependencyType where
  pretty _ IfOlderThan  = B.singleton ':'
  pretty _ Always       = B.singleton '!'
  pretty _ NoAccumulate = "::"

data ShellCmd = ShellCmd ![CommandMode] !Text
  deriving (Data, Show, Eq)

instance Pretty ShellCmd where
  pretty _ (ShellCmd modes cmd)
    = mconcat [ tab
              , mconcat $ pretty () <$> modes
              , B.fromText cmd
              ]

data CommandMode
  = NoEcho -- ^@\@@
  | Dry    -- ^@+@
  | IgnErr -- ^@-@
  deriving (Data, Show, Eq)

instance Pretty CommandMode where
  pretty _ NoEcho = B.singleton '@'
  pretty _ Dry    = B.singleton '+'
  pretty _ IgnErr = B.singleton '-'

data Directive
  = DInclude     !IncMode !IncLoc !Text
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
  pretty depth dir = go <> newline
    where
      go :: Builder
      go = case dir of
             DInclude mode loc file -> dot <> pprInclude mode loc file
             DError       msg       -> dot <> "error "          <> B.fromText msg
             DExport      vars      -> dot <> "export "         <> pretty () vars
             DExportEnv   vars      -> dot <> "export-env "     <> pretty () vars
             DExportLit   vars      -> dot <> "export-literal " <> pretty () vars
             DInfo        msg       -> dot <> "info "           <> B.fromText msg
             DUndef       var       -> dot <> "undef "          <> pretty () var
             DUnexport    vars      -> dot <> "unexport "       <> pretty () vars
             DUnexportEnv vars      -> dot <> "unexport-env"    <> pretty () vars
             DWarning     msg       -> dot <> "warning "        <> B.fromText msg
             DConditional cond      -> pprConditional cond
             DFor         for       -> pretty depth for

      dot :: Builder
      dot = dotSpace depth

      pprInclude :: IncMode -> IncLoc -> Text -> Builder
      pprInclude mode loc file
        = pretty () mode <> space <> pprFile
        where
          pprFile :: Builder
          pprFile = case loc of
                      System -> B.singleton '<' <> B.fromText file <> B.singleton '>'
                      User   -> B.singleton '"' <> B.fromText file <> B.singleton '"'

      pprConditional :: Conditional -> Builder
      pprConditional (Conditional {..})
        = mconcat [ pprBranches branches
                  , pprElse else_
                  , dot
                  , "endif"
                  ]

      pprBranches :: NonEmpty CondBranch -> Builder
      pprBranches (br :| brs)
        = pretty (True, depth) br <>
          mconcat (pretty (False, depth) <$> brs)

      pprElse :: Maybe Makefile -> Builder
      pprElse Nothing  = mempty
      pprElse (Just m) = mconcat [ dot
                                 , "else"
                                 , newline
                                 , pretty (depth + 1) m
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
    { branches :: !(NonEmpty CondBranch)
    , else_    :: !(Maybe Makefile) -- ^@.else@
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

instance Pretty CondBranch where
  -- |The 'Bool' value indicates whether the branch is the first one,
  -- e.g. @.if@ as opposed to @.elif@. The 'Int' value represents the
  -- current depth of nested directives.
  type Context CondBranch = (Bool, Int)
  pretty ctx@(_, depth) (CondBranch cond m)
    = mconcat [ pretty ctx cond
              , newline
              , pretty (depth + 1) m
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
    = dot <> go cond
    where
      dot :: Builder
      dot = dotSpace depth

      go :: Condition -> Builder
      go (If      expr) = mconcat [switch "if"     , space, pretty (False, ()) expr]
      go (Ifdef   expr) = mconcat [switch "ifdef"  , space, pretty (False, ()) expr]
      go (Ifndef  expr) = mconcat [switch "ifndef" , space, pretty (False, ()) expr]
      go (Ifmake  expr) = mconcat [switch "ifmake" , space, pretty (False, ()) expr]
      go (Ifnmake expr) = mconcat [switch "ifnmake", space, pretty (False, ()) expr]

      switch :: Builder -> Builder
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
  pretty (isNested, ctx) (Not e ) = maybeParens isNested $ B.singleton '!' <> pretty (True, ctx) e
  pretty (isNested, ctx) (Or  es) = maybeParens isNested . sconcat . NE.intersperse " || " $ pretty (True, ctx) <$> es
  pretty (isNested, ctx) (And es) = maybeParens isNested . sconcat . NE.intersperse " && " $ pretty (True, ctx) <$> es
  pretty (_       , ctx) (Expr e) = pretty ctx e

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
  pretty _ LT = B.singleton '<'
  pretty _ LE = "<="
  pretty _ GT = B.singleton '>'
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
  pretty _ (EDefined  var ) = "defined"  <> parens (pretty () var )
  pretty _ (EMake     tgt ) = "make"     <> parens (pretty () tgt )
  pretty _ (EEmpty    var ) = "empty"    <> parens (pretty () var )
  pretty _ (EExists   file) = "exists"   <> parens (pretty () file)
  pretty _ (ETarget   tgt ) = "target"   <> parens (pretty () tgt )
  pretty _ (ECommands tgt ) = "commands" <> parens (pretty () tgt )
  pretty _ (ECompare lhs mRhs)
    = case mRhs of
        Nothing        -> B.fromText lhs
        Just (op, rhs) -> mconcat
                          [ B.fromText lhs
                          , space
                          , pretty () op
                          , space
                          , B.fromText rhs
                          ]

data ForLoop = ForLoop ![Variable] !Text !Makefile
  deriving (Data, Show, Eq)

instance Pretty ForLoop where
  -- |The current depth of nested directives.
  type Context ForLoop = Int
  pretty depth (ForLoop vars expr body)
    = mconcat [ dotSpace depth
              , "for "
              , pretty () vars
              , " in "
              , pretty () expr
              , newline
              , pretty (depth + 1) body
              , dotSpace depth
              , "endfor"
              ]

infix 0 #
(#) :: Block -> Text -> Block
BBlank      b # c = BBlank      $ b { bComment = Just (Comment c) }
BAssignment a # c = BAssignment $ a { aComment = Just (Comment c) }
BRule       r # c = BRule       $ r { rComment = Just (Comment c) }
BDirective  d # _ = BDirective d
BUnindent   b # _ = BUnindent b

blank :: Block
blank = BBlank $ Blank Nothing

(.=) :: Variable -> [Text] -> Block
var .= tokens = BAssignment $ Assignment var Set tokens Nothing

(.+=) :: Variable -> [Text] -> Block
var .+= tokens = BAssignment $ Assignment var Append tokens Nothing

(.?=) :: Variable -> [Text] -> Block
var .?= tokens = BAssignment $ Assignment var SetIfUndefined tokens Nothing

(.:=) :: Variable -> [Text] -> Block
var .:= tokens = BAssignment $ Assignment var ExpandThenSet tokens Nothing

(.!=) :: Variable -> [Text] -> Block
var .!= tokens = BAssignment $ Assignment var ExecThenSet tokens Nothing

include :: Text -> Block
include file = BDirective $ DInclude Normal User file

-- | Construct a block which pretty prints with one less indentation
-- level. Used for printing unindented conditionals like
--
-- > .if ${foo} > 0
-- > .include "foo.mk"
-- > .endif
unindent :: Block -> Block
unindent = BUnindent

infix 1 ?==
(?==) :: Text -> Text -> Expr
a ?== b = ECompare a (Just (EQ, b))

prettyPrintAST :: Makefile -> TL.Text
prettyPrintAST = B.toLazyText . pretty 0

pprAssignments :: Foldable t => t Assignment -> Builder
pprAssignments as
  | pprNormally = foldr (\a -> (pretty alignment a <>)) mempty as
  | otherwise   = foldr (\a -> (pretty () (FoldedAssignment a) <>)) mempty as
  where
    alignment :: Int
    alignment = foldl' maxAlign 0 as

    maxAlign :: Int -> Assignment -> Int
    maxAlign n (Assignment {..})
      = let pre     = pretty () aVar <> pretty () aType
            len     = fromIntegral . TL.length . B.toLazyText $ pre
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
              , tab
              , if null aTokens && isNothing aComment
                then "# empty"
                else mconcat [ backslash
                             , newline
                             , go aTokens
                             , pprOptionalComment aComment
                             , newline
                             ]
              ]
    where
      go :: [Text] -> Builder
      go []     = error "impossible"
      go [x]    = tab <> B.fromText x
      go (x:xs) = tab <> B.fromText x <> tab <> backslash <> newline <> go xs
