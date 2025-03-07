{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Language.BMake.AST.Plain
  ( -- * AST variant tag
    PlainAST

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

    -- * Pretty-printing AST
  , prettyPrintMakefile
  ) where

import Data.Data (Data)
import Data.Foldable1 (foldlMap1')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isNothing)
import Data.Semigroup (sconcat, stimesMonoid)
import Data.Sequence (Seq, (|>))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Language.BMake.AST.Extension
import Language.BMake.AST.Pretty (Pretty(..))
import Language.BMake.AST.Types
import Prelude hiding (Ordering(..), exp)
import Prettyprinter ((<+>), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PT

-- |The type @'Makefile' 'PlainAST'@ is a variant of Makefile AST. It is
-- constructed programmatically and is pretty-printed as a Makefile. Unlike
-- "Language.BMake.AST.ExactPrint.ExactPrint" plain AST does not round-trip.
data PlainAST
  deriving (Data)

type instance XComment     PlainAST   = ()
type instance XBlank       PlainAST   = ()
type instance XAssignment  PlainAST   = ()
type instance XValue       PlainAST   = ()
type instance XDependency  PlainAST   = ()
type instance XShellCmd    PlainAST   = ()
type instance XInclude     PlainAST   = ()
type instance XMessage     PlainAST   = ()
type instance XExport      PlainAST   = ()
type instance XExportAll   PlainAST   = ()
type instance XUnexportEnv PlainAST   = ()
type instance XUndef       PlainAST   = ()
-- |Whether to indent the contents of this conditional. This should usually
-- be 'True'.
type instance XConditional PlainAST   = Bool
type instance XElse        PlainAST   = ()
type instance XEndIf       PlainAST   = ()
type instance XIf          PlainAST   = ()
type instance XIfdef       PlainAST   = ()
type instance XIfmake      PlainAST   = ()
type instance XNot         PlainAST   = ()
type instance XAnd         PlainAST   = ()
type instance XOr          PlainAST   = ()
type instance XExpr        PlainAST   = ()
type instance XExpLE       PlainAST a = Void
type instance XECompare    PlainAST   = ()
type instance XFor         PlainAST   = ()
type instance XEndFor      PlainAST   = ()
type instance XBreak       PlainAST   = ()

tabWidth :: Integral n => n
tabWidth = 8

tab :: Doc ann
tab = PP.pretty '\t'

dotSpace :: Int -> Doc ann
dotSpace depth = PP.dot <> stimesMonoid (depth * 2) PP.space

instance Pretty (Makefile PlainAST) where
  -- |The current depth of nested directives.
  type Context (Makefile PlainAST) = Int
  pretty depth = go mempty . blocks
    where
      -- Align consecutive assignments.
      go :: Seq (Assignment PlainAST) -> [Block PlainAST] -> Doc ann
      go as []     = pprAssignments as
      go as (b:bs) =
        case b of
          BBlank      x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BAssignment a -> go (as |> a) bs
          BRule       x -> pprAssignments as <> pretty ()    x <> go mempty bs
          BDirective  x -> pprAssignments as <> pretty depth x <> go mempty bs

-- |Comments in the plain AST are pretty-printed with a single space
-- between @#@ and the comment text.
instance Pretty (Comment PlainAST) where
  pretty _ (Comment _ c)
    = PP.pretty '#' <+> PP.pretty c

-- | Optionally render a comment at the end of a supposedly non-empty
-- line. There will be a TAB character inserted right before @#@.
pprOptionalComment :: Maybe (Comment PlainAST) -> Doc ann
pprOptionalComment = foldMap ((tab <>) . pretty ())

instance Pretty (Blank PlainAST) where
  pretty _ (Blank _ c) =
    foldMap (pretty ()) c <> PP.hardline

instance Pretty (Assignment PlainAST) where
  -- |The column number to align tokens.
  type Context (Assignment PlainAST) = Int
  pretty col (Assignment {..})
    = mconcat [ pretty () aVar <> pretty () aOp
              , if null aValues
                then foldMap ((tabs <>) . pretty ()) aComment
                else mconcat [ tabs
                             , PP.hsep $ pretty () <$> aValues
                             , pprOptionalComment aComment
                             ]
              , PP.hardline
              ]
    where
      tabs :: Doc ann
      tabs = PP.column $ \cur -> stimesMonoid (nTabs cur) tab

      nTabs :: Int -> Int
      nTabs cur = (max 0 (col - cur - 1) `div` tabWidth) + 1

instance Pretty (Value PlainAST) where
  pretty _ (Value _ v) = PP.pretty (escape v)
    where
      escape :: Text -> Text
      escape txt
        | T.any isUnsafe txt =
            T.concatMap (\c -> if isUnsafe c
                               then T.pack ['\\', c]
                               else T.singleton c) txt
        | otherwise = txt

      isUnsafe :: Char -> Bool
      isUnsafe '#'  = True
      isUnsafe '\\' = True
      isUnsafe _    = False

instance IsString (Value PlainAST) where
  fromString = Value () . fromString

instance Pretty (Rule PlainAST) where
  pretty _ (Rule {..}) =
    mconcat [ pretty () rDependency
            , mconcat $ pretty () <$> rCommands
            ]

instance Pretty (Dependency PlainAST) where
  pretty _ (Dependency {..})
    = mconcat [ foldlMap1' (pretty ()) ((. pretty ()) . (<+>)) dTargets -- hsep doesn't work on Foldable
              , pretty () dType
              , PP.space
              , PP.hsep $ pretty () <$> dSources
              , pprOptionalComment dComment
              , PP.hardline
              ]

instance Pretty (ShellCmd PlainAST) where
  pretty _ (ShellCmd {..})
    = mconcat [ tab
              , mconcat $ pretty () <$> sModes
              , pretty () sCommand
              , pprOptionalComment sComment
              , PP.hardline
              ]

instance Pretty (Directive PlainAST) where
  -- |The current depth of nested directives.
  type Context (Directive PlainAST) = Int
  pretty depth d
    | DInclude     inc <- d = pretty depth inc
    | DMessage     msg <- d = pretty depth msg
    | DExport      exp <- d = pretty depth exp
    | DExportAll   exa <- d = pretty depth exa
    | DUnexportEnv uxe <- d = pretty depth uxe
    | DUndef       und <- d = pretty depth und
    | DConditional con <- d = pretty depth con
    | DFor         for <- d = pretty depth for
    | DBreak       brk <- d = pretty depth brk

instance Pretty (Include PlainAST) where
  -- |The current depth of nested directives.
  type Context (Include PlainAST) = Int
  pretty depth (Include {..}) =
    mconcat [ dotSpace depth
            , pretty () iMode
            , PP.space
            , case iLoc of
                System -> PP.angles  . PP.pretty $ iPath
                User   -> PP.dquotes . PP.pretty $ iPath
            , pprOptionalComment iComment
            , PP.hardline
            ]

instance Pretty (Message PlainAST) where
  -- |The current depth of nested directives.
  type Context (Message PlainAST) = Int
  pretty depth (Message {..}) =
    mconcat [ dotSpace depth
            , case msgType of
                Info    -> "info"
                Warning -> "warning"
                Error   -> "error"
            , PP.space
            , pretty () msgText
            , pprOptionalComment msgComment
            , PP.hardline
            ]

instance Pretty (Export PlainAST) where
  -- |The current depth of nested directives.
  type Context (Export PlainAST) = Int
  pretty depth (Export {..}) =
    mconcat [ dotSpace depth
            , case expWay of
                Exp    -> "export"
                ExpEnv -> "export-env"
                ExpLit -> "export-literal"
                Unexp  -> "unexport"
            , PP.space
            , PP.hsep $ pretty () <$> expVars
            , pprOptionalComment expComment
            , PP.hardline
            ]

instance Pretty (ExportAll PlainAST) where
  -- |The current depth of nested directives.
  type Context (ExportAll PlainAST) = Int
  pretty depth (ExportAll _ com) =
    mconcat [ dotSpace depth
            , "export-all"
            , pprOptionalComment com
            , PP.hardline
            ]

instance Pretty (UnexportEnv PlainAST) where
  -- |The current depth of nested directives.
  type Context (UnexportEnv PlainAST) = Int
  pretty depth (UnexportEnv _ com) =
    mconcat [ dotSpace depth
            , "unexport-env"
            , pprOptionalComment com
            , PP.hardline
            ]

instance Pretty (Undef PlainAST) where
  -- |The current depth of nested directives.
  type Context (Undef PlainAST) = Int
  pretty depth (Undef _ vars com) =
    mconcat [ dotSpace depth
            , "undef"
            , PP.space
            , PP.hsep $ pretty () <$> vars
            , pprOptionalComment com
            , PP.hardline
            ]

instance Pretty (Conditional PlainAST) where
  -- |The current depth of nested directives.
  type Context (Conditional PlainAST) = Int
  pretty depth (Conditional {..}) =
    mconcat [ pprBranches condBranches
            , foldMap (pretty (depth, contentDepth)) condElse
            , pretty depth condEnd
            ]
    where
      contentDepth :: Int
      contentDepth
        | condExt   = depth + 1
        | otherwise = depth

      pprBranches :: NonEmpty (CondBranch PlainAST) -> Doc ann
      pprBranches (br :| brs) =
        pretty (True, contentDepth) br <>
        mconcat (pretty (False, contentDepth) <$> brs)

instance Pretty (Else PlainAST) where
  -- |The current and the content depth of nested directives.
  type Context (Else PlainAST) = (Int, Int)
  pretty (depth, contentDepth) (Else _ com mk) =
    PP.vsep [ dotSpace depth <> "else" <> pprOptionalComment com
            , pretty contentDepth mk
            ]

instance Pretty (EndIf PlainAST) where
  -- |The current depth of nested directives.
  type Context (EndIf PlainAST) = Int
  pretty depth (EndIf _ com) =
    mconcat [ dotSpace depth
            , "endif"
            , pprOptionalComment com
            , PP.hardline
            ]

instance Pretty (CondBranch PlainAST) where
  -- |The 'Bool' value indicates whether the branch is the first one,
  -- e.g. @.if@ as opposed to @.elif@. The 'Int' value represents the
  -- desired, not current, depth of nested directives.
  type Context (CondBranch PlainAST) = (Bool, Int)
  pretty ctx@(_, depth) (CondBranch cond m) =
    PP.vsep [ pretty ctx cond
            , pretty depth m
            ]

instance Pretty (Condition PlainAST) where
  type Context (Condition PlainAST) = (Bool, Int)
  pretty (isFirst, depth) cond
    = dotSpace depth <> go cond
    where
      go :: Condition PlainAST -> Doc ann
      go (If      _   expr com) = switch    "if"        <+> pretty (False, ()) expr <> pprOptionalComment com
      go (Ifdef   _ n expr com) = switch' n "if" "def"  <+> pretty (False, ()) expr <> pprOptionalComment com
      go (Ifmake  _ n expr com) = switch' n "if" "make" <+> pretty (False, ()) expr <> pprOptionalComment com

      switch :: Doc ann -> Doc ann
      switch d
        | isFirst   = d
        | otherwise = "el" <> d

      switch' :: Bool -> Doc ann -> Doc ann -> Doc ann
      switch' n d0 d1
        | isFirst   = d
        | otherwise = "el" <> d
        where
          d | n         = d0 <> "n" <> d1
            | otherwise = d0 <> d1

instance Pretty a => Pretty (LogicalExpr PlainAST a) where
  -- |Whether the expression is a nested one.
  type Context (LogicalExpr PlainAST a) = (Bool, Context a)
  pretty (isNested, ctx) le =
    case le of
      Not  _ e  -> maybeParens isNested $ PP.pretty '!' <> pretty (True, ctx) e
      Or   _ es -> maybeParens isNested . sconcat . NE.intersperse " || " $ pretty (True, ctx) <$> es
      And  _ es -> maybeParens isNested . sconcat . NE.intersperse " && " $ pretty (True, ctx) <$> es
      Expr _ e  -> pretty ctx e
    where
      maybeParens :: Bool -> Doc ann -> Doc ann
      maybeParens True  = PP.parens
      maybeParens False = id

instance Pretty (Expr PlainAST) where
  pretty _ (EDefined  var ) = "defined"  <> PP.parens (pretty () var )
  pretty _ (EMake     tgt ) = "make"     <> PP.parens (pretty () tgt )
  pretty _ (EEmpty    var ) = "empty"    <> PP.parens (pretty () var )
  pretty _ (EExists   file) = "exists"   <> PP.parens (pretty () file)
  pretty _ (ETarget   tgt ) = "target"   <> PP.parens (pretty () tgt )
  pretty _ (ECommands tgt ) = "commands" <> PP.parens (pretty () tgt )
  pretty _ (ECompare {..})
    = case eCmpRHS of
        Nothing        -> pretty () eCmpLHS
        Just (op, rhs) -> PP.hsep [ pretty () eCmpLHS
                                  , pretty () op
                                  , pretty () rhs
                                  ]

instance Pretty (ForLoop PlainAST) where
  -- |The current depth of nested directives.
  type Context (ForLoop PlainAST) = Int
  pretty depth (ForLoop for body end) =
    mconcat [ pretty depth for
            , pretty (depth + 1) body
            , pretty depth end
            ]

instance Pretty (For PlainAST) where
  -- |The current depth of nested directives.
  type Context (For PlainAST) = Int
  pretty depth (For {..}) =
    mconcat [ dotSpace depth
            , "for"
            , PP.space
            , foldlMap1' (pretty ()) ((. pretty ()) . (<+>)) forVars -- hsep doesn't work on Foldable
            , PP.space
            , "in"
            , PP.space
            , pretty () forExpr
            , pprOptionalComment forComment
            , PP.hardline
            ]

instance Pretty (EndFor PlainAST) where
  -- |The current depth of nested directives.
  type Context (EndFor PlainAST) = Int
  pretty depth (EndFor _ com) =
    mconcat [ dotSpace depth
            , "endfor"
            , pprOptionalComment com
            , PP.hardline
            ]

instance Pretty (Break PlainAST) where
  -- |The current depth of nested directives.
  type Context (Break PlainAST) = Int
  pretty depth (Break _ com) =
    mconcat [ dotSpace depth
            , "break"
            , pprOptionalComment com
            , PP.hardline
            ]

pprAssignments :: Foldable t => t (Assignment PlainAST) -> Doc ann
pprAssignments as
  | pprNormally = foldr (\a -> (pretty alignment a <>)) mempty as
  | otherwise   = foldr (\a -> (pretty () (FoldedAssignment a) <>)) mempty as
  where
    alignment :: Int
    alignment = foldl' maxAlign 0 as

    maxAlign :: Int -> Assignment PlainAST -> Int
    maxAlign n (Assignment {..}) =
      -- This is essentially a two-pass operation, so we cannot implement
      -- it in terms of things like PP.width
      let pre     = pretty () aVar <> pretty () aOp
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
newtype FoldedAssignment = FoldedAssignment (Assignment PlainAST)

instance Pretty FoldedAssignment where
  pretty _ (FoldedAssignment (Assignment {..}))
    = mconcat [ pretty () aVar
              , pretty () aOp
              , if null aValues && isNothing aComment
                then PP.hardline
                else mconcat [ tab
                             , PP.backslash
                             , PP.hardline
                             , go aValues
                             , pprOptionalComment aComment
                             , PP.hardline
                             ]
              ]
    where
      go :: [Value PlainAST] -> Doc ann
      go []     = error "impossible"
      go [x]    = tab <> pretty () x
      go (x:xs) = mconcat [ tab
                          , pretty () x
                          , tab
                          , PP.backslash
                          , PP.hardline
                          , go xs
                          ]

infix 0 #
-- |Attach a comment to a block.
(#) :: Block PlainAST -> Text -> Block PlainAST
BBlank      b # c = BBlank      $ b { bComment = Just (Comment () c) }
BAssignment a # c = BAssignment $ a { aComment = Just (Comment () c) }
-- FIXME: This isn't quite correct. We should turn (#) into a method in
-- some class.
BRule       r # _ = BRule      r
-- FIXME: Same as above.
BDirective  d # _ = BDirective d

blank :: Block PlainAST
blank = BBlank $ Blank () Nothing

infix 1 .=
(.=) :: Value PlainAST -> [Text] -> Block PlainAST
var .= tokens =
  BAssignment $ Assignment () var Set (Value () <$> tokens) Nothing

infix 1 .+=
(.+=) :: Value PlainAST -> [Text] -> Block PlainAST
var .+= tokens =
  BAssignment $ Assignment () var Append (Value () <$> tokens) Nothing

infix 1 .?=
(.?=) :: Value PlainAST -> [Text] -> Block PlainAST
var .?= tokens =
  BAssignment $ Assignment () var SetIfUndefined (Value () <$> tokens) Nothing

infix 1 .:=
(.:=) :: Value PlainAST -> [Text] -> Block PlainAST
var .:= tokens =
  BAssignment $ Assignment () var ExpandThenSet (Value () <$> tokens) Nothing

infix 1 .!=
(.!=) :: Value PlainAST -> [Text] -> Block PlainAST
var .!= tokens =
  BAssignment $ Assignment () var ExecThenSet (Value () <$> tokens) Nothing

include :: Text -> Block PlainAST
include path =
  BDirective . DInclude $ Include () Normal User path Nothing

infix 1 ?==
(?==) :: Text -> Text -> Expr PlainAST
a ?== b = ECompare () (Value () a) (Just (EQ, Value () b))

prettyPrintMakefile :: Makefile PlainAST -> TL.Text
prettyPrintMakefile = PT.renderLazy . PP.layoutCompact . pretty 0
