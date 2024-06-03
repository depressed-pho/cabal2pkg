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
  , LogicalOp(..)
  , RelationalOp(..)
  , Expr(..)

    -- * Construction
  , (#)
  , blank
  , (.=)
  , (.+=)
  , include

    -- * Pretty-printing
  , prettyPrintAST
  ) where

import Data.Foldable (foldl')
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Semigroup (stimesMonoid)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B


class Pretty a where
  type Context a
  type instance Context a = ()
  pretty :: Context a -> a -> Builder

instance Pretty a => Pretty [a] where
  type Context [a] = Context a
  pretty ctx = mconcat . intersperse space . (pretty ctx <$>)

instance Pretty Text where
  pretty _ = B.fromText

tabWidth :: Integral n => n
tabWidth = 8

space :: Builder
space = B.singleton ' '

newline :: Builder
newline = B.singleton '\n'

tab :: Builder
tab = B.singleton '\t'

newtype Makefile = Makefile { blocks :: [Block] }
  deriving (Show, Eq, Semigroup, Monoid)

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

newtype Comment = Comment Text
  deriving (Show, Eq, IsString)

instance Pretty Comment where
  pretty _ (Comment c)
    = "# " <> B.fromText c

data Block
  = BBlank      !Blank
  | BAssignment !Assignment
  | BRule       !Rule
  | BDirective  !Directive
  deriving (Show, Eq)

newtype Variable = Variable Text
  deriving (Show, Eq, IsString)

instance Pretty Variable where
  pretty _ (Variable name)
    = B.fromText name

newtype Target = Target Text
  deriving (Show, Eq, IsString)

instance Pretty Target where
  pretty _ (Target path)
    = B.fromText path

-- |A blank line.
newtype Blank = Blank { bComment :: (Maybe Comment) }
  deriving (Show, Eq)

instance Pretty Blank where
  pretty _ (Blank c)
    = maybe mempty (pretty ()) c <> newline

data Assignment
  = Assignment
    { aVar     :: !Variable
    , aType    :: !AssignmentType
    , aTokens  :: ![Text]
    , aComment :: !(Maybe Comment)
    }
  deriving (Show, Eq)

instance Pretty Assignment where
  -- |The column number to align tokens.
  type Context Assignment = Int
  pretty col (Assignment {..})
    = mconcat [ pre
              , stimesMonoid nTabs tab
              , pretty () aTokens
              , maybe mempty ((space <>) . pretty ()) aComment
              , newline
              ]
    where
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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

instance Pretty Rule where
  pretty _ (Rule {..})
    = mconcat [ pretty () rDependency
              , maybe mempty (pretty ()) rComment
              , newline
              , mconcat $ pretty () <$> rCommands
              ]

data Dependency
  = Dependency
    { dTargets :: ![Target]
    , dType    :: !DependencyType
    , dSources :: ![Text]
    }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

instance Pretty DependencyType where
  pretty _ IfOlderThan  = B.singleton ':'
  pretty _ Always       = B.singleton '!'
  pretty _ NoAccumulate = "::"

data ShellCmd = ShellCmd ![CommandMode] !Text
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  | DFor         ![Variable] !Text -- ^@.for@
  deriving (Show, Eq)

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
             DConditional cond      -> error ("FIXME: " <> show cond)
             DFor         vars expr -> error ("FIXME: .for " <> show vars <> " in " <> show expr)

      dot :: Builder
      dot = B.singleton '.' <> stimesMonoid depth space

      pprInclude :: IncMode -> IncLoc -> Text -> Builder
      pprInclude mode loc file
        = pretty () mode <> space <> pprFile
        where
          pprFile :: Builder
          pprFile = case loc of
                      System -> B.singleton '<' <> B.fromText file <> B.singleton '>'
                      User   -> B.singleton '"' <> B.fromText file <> B.singleton '"'


data IncMode
  = Normal -- ^@.include@
  | SMode  -- ^@.sinclude@
  | DMode  -- ^@.dinclude@
  deriving (Show, Eq)

instance Pretty IncMode where
  pretty _ Normal = "include"
  pretty _ SMode  = "sinclude"
  pretty _ DMode  = "dinclude"

data IncLoc
  = System -- ^@.include <FILE>@
  | User   -- ^@.include "FILE"@
  deriving (Show, Eq)

data Conditional
  = Conditional
    { branches  :: ![CondBranch]
    , otherwise :: !(Maybe Makefile) -- ^@.else@
    }
  deriving (Show, Eq)

data CondBranch = CondBranch !Condition !Makefile
  deriving (Show, Eq)

data Condition
  = If        !Bool !Expr     ![(LogicalOp, Expr    )] -- ^@.if [!]EXPR [OP EXPR ...]@
  | Ifdef     !Bool !Variable ![(LogicalOp, Variable)] -- ^@.ifdef [!]VARIABLE [OP VARIABLE ...]@
  | Ifndef    !Bool !Variable ![(LogicalOp, Variable)] -- ^@.ifndef [!]VARIABLE [OP VARIABLE ...]@
  | Ifmake    !Bool !Target   ![(LogicalOp, Target  )] -- ^@.ifmake [!]TARGET [OP TARGET ...]@
  | Ifnmake   !Bool !Target   ![(LogicalOp, Target  )] -- ^@.ifnmake [!]TARGET [OP TARGET ...]@
  deriving (Show, Eq)

data LogicalOp
  = OR  -- ^@||@
  | AND -- ^@&&@
  deriving (Show, Eq)

data RelationalOp
  = EQ -- ^@==@
  | NE -- ^@!=@
  | LT -- ^@<@
  | LE -- ^@<=@
  | GT -- ^@>@
  | GE -- ^@>=@
  deriving (Show, Eq)

data Expr
  = EDefined  !Variable -- ^@defined(VARIABLE)@
  | EMake     !Target   -- ^@make(TARGET)@
  | EEmpty    !Text     -- ^@empty(VARIABLE)@
  | EExists   !Text     -- ^@exists(FILE)@
  | ETarget   !Target   -- ^@target(TARGET)@
  | ECommands !Target   -- ^@commands(TARGET)@
  | ECompare  !Text !(Maybe (RelationalOp, Text))
  deriving (Show, Eq)

infix 0 #
(#) :: Block -> Text -> Block
BBlank      b # c = BBlank      $ b { bComment = Just (Comment c) }
BAssignment a # c = BAssignment $ a { aComment = Just (Comment c) }
BRule       r # c = BRule       $ r { rComment = Just (Comment c) }
BDirective  d # _ = BDirective d

blank :: Block
blank = BBlank $ Blank Nothing

(.=) :: Variable -> [Text] -> Block
var .= tokens = BAssignment $ Assignment var Set tokens Nothing

(.+=) :: Variable -> [Text] -> Block
var .+= tokens = BAssignment $ Assignment var Append tokens Nothing

include :: Text -> Block
include file = BDirective $ DInclude Normal User file

prettyPrintAST :: Makefile -> TL.Text
prettyPrintAST = B.toLazyText . pretty 0

pprAssignments :: Foldable t => t Assignment -> Builder
pprAssignments as = foldr (\a -> (pretty alignment a <>)) mempty as
  where
    alignment :: Int
    alignment = foldl' go 0 as

    go :: Int -> Assignment -> Int
    go n (Assignment {..})
      = let pre     = pretty () aVar <> pretty () aType
            len     = fromIntegral . TL.length . B.toLazyText $ pre
            aligned = ((len `div` tabWidth) + 1) * tabWidth
        in
          max n aligned
