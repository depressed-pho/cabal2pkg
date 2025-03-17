{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Conditional
  ( Environment(..)
  , CondBlock(..), always, conds
  , Conditional(..), branches, condElse
  , CondBranch(..), condition, condBlock
  , Condition(..)
  , extractCondBlock
  ) where

import Cabal2Pkg.CmdLine (FlagMap)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Data (Data)
import Data.Filtrable (mapMaybe)
import Data.IsNull (IsNull(..))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Distribution.Compiler qualified as C
import Distribution.System qualified as C
import Distribution.Types.CondTree qualified as C
import Distribution.Types.Condition qualified as C
import Distribution.Types.ConfVar qualified as C
import Distribution.Types.Flag qualified as C
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, withinRange)
import GHC.Generics (Generic, Generically(..))
import GHC.Stack (HasCallStack)
import Language.BMake.AST.Plain ((?==), PlainAST)
import Language.BMake.AST qualified as AST
import Lens.Micro.Platform ((&), (^.), (%~), (.~), makeLenses, traversed)
import Prelude hiding (filter)
import UnliftIO.Async (Conc, runConc)

class Simplifiable a where
  simplify :: a -> a

class GarbageCollectable a where
  garbageCollect :: a -> a

data Environment = Environment
  { flags      :: !FlagMap
  , ghcVersion :: !Version
  }
  deriving Show

-- |This type represents a block of Cabal conditional.
--
-- > if flag(foo)
-- >   build-depends: foo
-- >
-- >   if flag(bar)
-- >     build-depends: bar
-- >
-- >   if flag(baz)
-- >     build-depends: baz
-- >   else
-- >     build-depends: qux
--
-- The example Cabal conditional above is represented as follows:
--
-- > CondBlock
-- > { _always = {- build-depends: foo -}
-- > , _conds  =
-- >     [ Conditional
-- >       { _branches =
-- >           [ CondBranch
-- >             { _condition = {- flag(bar) -}
-- >             , _block     =
-- >                 CondBlock
-- >                 { _always = {- build-depends: bar -}
-- >                 , _conds  = []
-- >                 }
-- >             }
-- >           ]
-- >       , _else = Nothing
-- >       }
-- >     , Conditional
-- >       { _branches =
-- >           [ CondBranch
-- >             { _condition = {- flag(baz) -}
-- >             , _block     =
-- >                 CondBlock
-- >                 { _always = {- build-depends: baz -}
-- >                 , _conds  = []
-- >                 }
-- >             }
-- >           ]
-- >       , _else =
-- >           Just ( CondBranch
-- >                  { _always = {- build-depends: qux -}
-- >                  , _conds  = []
-- >                  }
-- >                )
-- >       }
-- >     ]
-- > }
--
-- Note that 'Conditional's in a 'CondBlock' are completely independent to
-- each other. They don't form a "if ... elif" relationship. "elif" is
-- represented by 'Conditional' having more than a single 'CondBranch'.
data CondBlock a = CondBlock
  { _always :: !a
  , _conds  :: ![Conditional a]
  }
  deriving (Data, Eq, Functor, Generic, Show)
  deriving (Monoid, Semigroup) via Generically (CondBlock a)

data Conditional a = Conditional
  { _branches :: ![CondBranch a]
  , _condElse :: !(Maybe (CondBlock a))
  }
  deriving (Data, Eq, Functor, Generic, Show)

data CondBranch a = CondBranch
  { _condition :: !Condition
  , _condBlock :: !(CondBlock a)
  }
  deriving (Data, Eq, Functor, Generic, Show)

-- |An intermediate data type for Cabal conditions.
data Condition
  = Literal !Bool
  | Not !Condition
  | Or  !Condition !Condition
  | And !Condition !Condition
  | Expr
    { expression :: !(AST.Expr PlainAST)
      -- |'True' iff the expression depends on variables defined in
      -- @../../mk/bsd.fast.prefs.mk@
    , needsPrefs :: !Bool
    }
  deriving (Data, Eq, Show)

makeLenses ''CondBlock
makeLenses ''Conditional
makeLenses ''CondBranch

instance IsNull a => IsNull (CondBlock a) where
  isNull bl =
        isNull (bl ^. always) &&
    all isNull (bl ^. conds )

instance IsNull a => IsNull (Conditional a) where
  isNull cd =
    all isNull (cd ^. branches) &&
    all isNull (cd ^. condElse)

instance IsNull a => IsNull (CondBranch a) where
  isNull = isNull . (^. condBlock)

-- |Conditionals in the Cabal AST is fucking irritatingly
-- incomprehensible. I simply cannot get the point of the type variable @c@
-- in @CondTree v c a@. Isn't the fucking condTreeConstraints already
-- contained in @condTreeData :: a@? To confuse people trying to work with
-- the fucking AST?
extractCondBlock :: forall m a a' _c c' .
                    ( HasCallStack
                    , IsNull c'
                    , MonadUnliftIO m
                    , Monoid c'
                    , Semigroup (m c')
                    )
                 => (a -> Conc m c')
                 -> (a -> CondBlock c' -> a')
                 -> Environment
                 -> C.CondTree C.ConfVar _c a
                 -> m a'
extractCondBlock extractContent extractOuter env = go
  where
    go :: HasCallStack => C.CondTree C.ConfVar _c a -> m a'
    go tree =
      do block <- (floatConditionals . garbageCollect <$>)
                  . runConc
                  . forceBlock
                  . simplify
                  $ mkBlock tree
         pure $ extractOuter (C.condTreeData tree) block

    forceBlock :: HasCallStack => CondBlock (Conc m c') -> Conc m (CondBlock c')
    forceBlock bl =
      CondBlock
      <$> bl ^. always
      <*> traverse forceConditional (bl ^. conds)

    forceConditional :: HasCallStack => Conditional (Conc m c') -> Conc m (Conditional c')
    forceConditional cd =
      Conditional
      <$> traverse forceBranch (cd ^. branches)
      <*> traverse forceBlock  (cd ^. condElse)

    forceBranch :: HasCallStack => CondBranch (Conc m c') -> Conc m (CondBranch c')
    forceBranch br =
      CondBranch (br ^. condition)
      <$> forceBlock (br ^. condBlock)

    mkBlock :: HasCallStack
            => C.CondTree C.ConfVar _c a
            -> CondBlock (Conc m c')
    mkBlock tree =
      CondBlock
      { _always = extractContent $ C.condTreeData tree
      , _conds  = extractConditional <$> C.condTreeComponents tree
      }

    extractConditional :: HasCallStack
                       => C.CondBranch C.ConfVar _c a
                       -> Conditional (Conc m c')
    extractConditional branch =
      Conditional
      { _branches = [ CondBranch
                      { _condition = extractCondition $ C.condBranchCondition branch
                      , _condBlock = mkBlock $ C.condBranchIfTrue branch
                      }
                    ]
      , _condElse = mkBlock <$> C.condBranchIfFalse branch
      }

    extractCondition :: HasCallStack => C.Condition C.ConfVar -> Condition
    extractCondition c
      = case c of
          C.Var  var   -> extractVarCond var
          C.Lit  b     -> Literal b
          C.CNot c'    -> Not (extractCondition c')
          C.COr  ca cb -> Or  (extractCondition ca) (extractCondition cb)
          C.CAnd ca cb -> And (extractCondition ca) (extractCondition cb)

    extractVarCond :: HasCallStack => C.ConfVar -> Condition
    extractVarCond var
      = case var of
          C.OS os                   -> extractOSCond os
          C.Arch arch               -> extractArchCond arch
          C.PackageFlag flag        -> extractFlagCond flag
          C.Impl cFlavour cVerRange -> extractImplCond cFlavour cVerRange

    extractFlagCond :: HasCallStack => C.FlagName -> Condition
    extractFlagCond flag
      = case M.lookup flag (flags env) of
          Just b  -> Literal b
          Nothing -> error $ "Undefined flag `" <> C.unFlagName flag <>
                             "' appeared in a condition"

    extractImplCond :: C.CompilerFlavor -> VersionRange -> Condition
    extractImplCond cFlavour cVerRange
      = case cFlavour of
          C.GHC -> Literal $ withinRange (ghcVersion env) cVerRange
          _     -> Literal False -- not supported by pkgsrc

extractOSCond :: C.OS -> Condition
extractOSCond os
  = case os of
      C.Linux     -> c "Linux"
      -- Windows is special. It's one of a few POSIX incompatible OSes and
      -- requires a lot of glue code to run regular Haskell
      -- programs. pkgsrc doesn't support Windows. We can safely assume
      -- we're not on it.
      C.Windows   -> Literal False
      C.OSX       -> c "Darwin"
      C.FreeBSD   -> c "FreeBSD"
      C.OpenBSD   -> c "OpenBSD"
      C.NetBSD    -> c "NetBSD"
      C.DragonFly -> c "DragonFly"
      C.Solaris   -> c "SunOS"
      C.AIX       -> c "AIX"
      C.HPUX      -> c "HPUX"
      C.IRIX      -> c "IRIX"
      C.HaLVM     -> Literal False -- not supported by pkgsrc
      C.Hurd      -> Literal False
      C.IOS       -> Literal False
      C.Android   -> Literal False
      C.Ghcjs     -> Literal False
      C.Wasi      -> Literal False
      C.Haiku     -> c "Haiku"
      C.OtherOS _ -> Literal False
  where
    c :: Text -> Condition
    c osName
      = Expr
        { expression = "${OPSYS}" ?== "\"" <> osName <> "\""
        , needsPrefs = True
        }

extractArchCond :: C.Arch -> Condition
extractArchCond arch
  = case arch of
      C.I386        -> c "i386"
      C.X86_64      -> c "x86_64"
      C.PPC         -> c "powerpc"
      C.PPC64       -> c "powerpc64"
      C.PPC64LE     -> c "powerpc64le"
      C.Sparc       -> c "sparc"
      C.Sparc64     -> c "sparc64"
      C.Arm         -> Not (c' $ AST.EEmpty "MACHINE_ARCH:M*arm*")
      C.AArch64     -> c "aarch64"
      C.Mips        -> Not (c' $ AST.EEmpty "MACHINE_ARCH:Mmips*")
      C.SH          -> Not (c' $ AST.EEmpty "MACHINE_ARCH:Msh3*")
      C.IA64        -> c "ia64"
      C.S390        -> Literal False -- not supported by pkgsrc
      C.S390X       -> Literal False
      C.Alpha       -> c "alpha"
      C.Hppa        -> c "hppa"
      C.Rs6000      -> Literal False
      C.M68k        -> c "m68k"
      C.Vax         -> c "vax"
      C.RISCV64     -> c "riscv64"
      C.LoongArch64 -> Literal False -- not supported by pkgsrc
      C.JavaScript  -> Literal False
      C.Wasm32      -> Literal False
      C.OtherArch _ -> Literal False
  where
    c :: Text -> Condition
    c archName
      = c' $ "${MACHINE_ARCH}" ?== "\"" <> archName <> "\""

    c' :: AST.Expr PlainAST -> Condition
    c' expr
      = Expr
        { expression = expr
        , needsPrefs = True
        }

-- |For each 'Conditional' in a 'CondBlock', if it has no branches, merge
-- its 'condElse' to the block. If it has a single 'CondBranch' whose
-- 'condition' can be folded to a constant, and if it folds to 'True' merge
-- its 'condBlock' back to the parent block and discard the 'condElse' of
-- the 'Conditional'. If it folds to 'False' we do the opposite.
instance Monoid a => Simplifiable (CondBlock a) where
  simplify block = bl0 <> foldr go mempty (block ^. conds)
    where
      bl0 :: CondBlock a
      bl0 = block & conds .~ []

      go :: Conditional a -> CondBlock a -> CondBlock a
      go cd bl
        | [] <- cd' ^. branches =
            maybe bl (<> bl) (cd' ^. condElse)
        | (br:[]) <- cd' ^. branches =
            case br ^. condition of
              Literal True  -> br ^. condBlock <> bl
              Literal False -> maybe bl (<> bl) (cd' ^. condElse)
              _             -> bl & conds %~ (cd':)
        | otherwise =
            bl & conds %~ (cd':)
        where
          cd' :: Conditional a
          cd' = simplify cd

-- |For each 'CondBranch' in a 'Conditional', see if its 'condition' can be
-- folded to a constant. If it folds to 'True' discard any of the
-- subsequent branches (i.e. @.elif@) and 'condElse' (i.e. @.else@). If it
-- folds to 'False' discard that specific branch.
instance Monoid a => Simplifiable (Conditional a) where
  simplify cond = go cd0 (cond ^. branches)
    where
      cd0 :: Conditional a
      cd0 = cond & branches .~ []
                 & condElse . traversed %~ simplify

      go :: Conditional a -> [CondBranch a] -> Conditional a
      go cd []       = cd
      go cd (br:brs) =
        let cd' = go cd brs
            br' = simplify br
        in case br' ^. condition of
             Literal True  -> cd' & branches .~ [br']
                                  & condElse .~ Nothing
             Literal False -> cd'
             _             -> cd' & branches %~ (br':)

-- |For each 'Conditional' in a 'CondBlock', see if its 'condElse' block
-- has an empty 'always' part and a single 'Conditional'. If that's the
-- case we can merge the block into its parent 'Conditional'. That is, we
-- turn this:
--
-- > .if AAA
-- > aaa
-- > .else
-- > .  if BBB
-- > bbb
-- > .  else
-- > ccc
-- > .  endif
-- > .endif
--
-- into this:
--
-- > .if AAA
-- > aaa
-- > .elif BBB
-- > bbb
-- > .else
-- > ccc
-- > .endif
floatConditionals :: IsNull a => CondBlock a -> CondBlock a
floatConditionals = conds . traversed %~ floatBranches

floatBranches :: IsNull a => Conditional a -> Conditional a
floatBranches cd
  | cd' <- cd & branches . traversed . condBlock %~ floatConditionals =
      case cd' ^. condElse of
        Just bl
          | isNull (bl ^. always)
          , bl'       <- floatConditionals bl
          , (cd'':[]) <- bl' ^. conds ->
              cd' & branches %~ (<> cd'' ^. branches)
                  & condElse .~ (   cd'' ^. condElse)
        _ -> cd'

-- |Remove any conditionals that are empty.
instance (IsNull a, Monoid a) => GarbageCollectable (CondBlock a) where
  garbageCollect = simplify . (conds %~ mapMaybe f)
    where
      f :: Conditional a -> Maybe (Conditional a)
      f cd
        | isNull cd' = Nothing
        | otherwise  = Just cd'
        where
          cd' :: Conditional a
          cd' = garbageCollect cd

instance Monoid a => Simplifiable (CondBranch a) where
  simplify = (condition %~ simplify)
           . (condBlock %~ simplify)

-- |Remove any branches whose 'CondBlock' is empty, ignoring their
-- 'Condition'.
--
-- Why do we have separate type classes for 'simplify' and
-- 'garbageCollect'? Because the latter can only be done after forcing
-- deferred monadic computations. We cannot test if @'Monad' m => m a@ is
-- empty just because @a@ is 'IsNull', without forcing the computation.
instance (IsNull a, Monoid a) => GarbageCollectable (Conditional a) where
  garbageCollect = (branches %~ mapMaybe f)
                 . (condElse %~ mapMaybe g)
    where
      f :: CondBranch a -> Maybe (CondBranch a)
      f br
        | isNull br' = Nothing
        | otherwise  = Just br'
        where
          br' :: CondBranch a
          br' = garbageCollect br

      g :: CondBlock a -> Maybe (CondBlock a)
      g bl
        | isNull bl' = Nothing
        | otherwise  = Just bl'
        where
          bl' :: CondBlock a
          bl' = garbageCollect bl

instance (IsNull a, Monoid a) => GarbageCollectable (CondBranch a) where
  garbageCollect = condBlock %~ garbageCollect

instance Simplifiable Condition where
  simplify c@(Literal _) = c

  simplify (Not c) =
    case simplify c of
      Literal b ->
        Literal (not b)

      Not c' ->
        -- Eliminate "Not Not".
        c'

      e@(Expr {..})
        | Just e' <- negateExpr expression ->
            -- If the immediate subexpression of Not is a comparison, we
            -- can eliminate the Not operator by negating the
            -- comparison. This is what pkglint wants us to do.
            e { expression = e' }

      c' ->
        Not c'

  simplify (Or a b)
    = case simplify a of
        Literal True  -> Literal True
        Literal False -> simplify b
        a'            ->
          case simplify b of
            Literal True  -> Literal True
            Literal False -> a'
            b'            -> Or a' b'

  simplify (And a b)
    = case simplify a of
        Literal True  -> simplify b
        Literal False -> Literal False
        a'            ->
          case simplify b of
            Literal True  -> a'
            Literal False -> Literal False
            b'            -> And a' b'

  simplify c@(Expr {}) = c

negateExpr :: AST.Expr PlainAST -> Maybe (AST.Expr PlainAST)
negateExpr cmp@(AST.ECompare {..})
  | Just (op, rhs) <- eCmpRHS =
      let op' = case op of
                  AST.EQ -> AST.NE
                  AST.NE -> AST.EQ
                  AST.LT -> AST.GE
                  AST.LE -> AST.GT
                  AST.GT -> AST.LE
                  AST.GE -> AST.LT
      in
        Just $ cmp { AST.eCmpRHS = Just (op', rhs) }
negateExpr _ = Nothing
