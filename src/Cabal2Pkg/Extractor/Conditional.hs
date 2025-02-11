{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Extractor.Conditional
  ( Environment(..)
  , CondBlock(..), always, branches
  , CondBranch(..), condition, ifTrue, ifFalse
  , Condition(..)
  , extractCondBlock
  ) where

import Cabal2Pkg.CmdLine (FlagMap)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Data (Data)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
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
import Language.BMake.AST ((?==))
import Language.BMake.AST qualified as AST
import Lens.Micro.Platform ((&), (^.), (%~), (.~), makeLenses)
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

data CondBlock a = CondBlock
  { _always   :: !a
  , _branches :: ![CondBranch a]
  }
  deriving (Data, Eq, Functor, Foldable, Generic, Show, Traversable)
  deriving (Monoid, Semigroup) via Generically (CondBlock a)

data CondBranch a = CondBranch
  { _condition :: !Condition
  , _ifTrue    :: !(CondBlock a)
  , _ifFalse   :: !(Maybe (CondBlock a))
  }
  deriving (Data, Eq, Functor, Foldable, Generic, Show, Traversable)

-- |An intermediate data type for Cabal conditions.
data Condition
  = Literal !Bool
  | Not !Condition
  | Or  !Condition !Condition
  | And !Condition !Condition
  | Expr
    { expression :: !AST.Expr
      -- |'True' iff the expression depends on variables defined in
      -- @../../mk/bsd.fast.prefs.mk@
    , needsPrefs :: !Bool
    }
  deriving (Data, Eq, Show)

makeLenses ''CondBlock
makeLenses ''CondBranch


-- |Conditionals in the Cabal AST is fucking irritatingly
-- incomprehensible. I simply cannot get the point of the type variable @c@
-- in @CondTree v c a@. Isn't the fucking condTreeConstraints already
-- contained in @condTreeData :: a@? To confuse people trying to work with
-- the fucking AST?
extractCondBlock :: forall m a a' _c c' .
                    ( HasCallStack
                    , MonadUnliftIO m
                    , Semigroup (m c')
                    , Monoid c'
                    , Eq c'
                    )
                 => (a -> Conc m c')
                 -> (a -> CondBlock c' -> a')
                 -> Environment
                 -> C.CondTree C.ConfVar _c a
                 -> m a'
extractCondBlock extractContent extractOuter env = go
  where
    go :: HasCallStack => C.CondTree C.ConfVar _c a -> m a'
    go tree
      = do block <- (floatBranches . garbageCollect <$>)
                    . runConc
                    . forceBlock
                    . simplify
                    $ mkBlock tree
           pure $ extractOuter (C.condTreeData tree) block

    forceBlock :: HasCallStack => CondBlock (Conc m c') -> Conc m (CondBlock c')
    forceBlock bl
      = CondBlock
        <$> bl ^. always
        <*> traverse forceBranch (bl ^. branches)

    forceBranch :: HasCallStack => CondBranch (Conc m c') -> Conc m (CondBranch c')
    forceBranch br
      = CondBranch (br ^. condition)
        <$> forceBlock (br ^. ifTrue)
        <*> traverse forceBlock (br ^. ifFalse)

    mkBlock :: HasCallStack
            => C.CondTree C.ConfVar _c a
            -> CondBlock (Conc m c')
    mkBlock tree
      = CondBlock
        { _always   = extractContent $ C.condTreeData tree
        , _branches = extractBranch <$> C.condTreeComponents tree
        }

    extractBranch :: HasCallStack
                  => C.CondBranch C.ConfVar _c a
                  -> CondBranch (Conc m c')
    extractBranch branch
      = CondBranch
        { _condition = extractCondition $ C.condBranchCondition branch
        , _ifTrue    = mkBlock $ C.condBranchIfTrue branch
        , _ifFalse   = mkBlock <$> C.condBranchIfFalse branch
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
      -- Windows is special. It's one of a few non-POSIX compatible OS and
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

    c' :: AST.Expr -> Condition
    c' expr
      = Expr
        { expression = expr
        , needsPrefs = True
        }

-- |For each branch in a 'CondBlock', see if its 'condition' can be folded
-- to a constant. If it folds to 'True' merge its 'ifTrue' back to the
-- parent node and discard its 'ifFalse'. If it folds to 'False' we do the
-- opposite. This means @'CondBlock' a@ has to form a semigroup.
instance Semigroup a => Simplifiable (CondBlock a) where
  simplify block
    = foldl' go (block & branches .~ []) (block ^. branches)
    where
      go :: CondBlock a -> CondBranch a -> CondBlock a
      go bl br
        = let br' = simplify br
          in case simplify $ br' ^. condition of
               Literal True  -> bl <> br' ^. ifTrue
               Literal False -> maybe bl (bl <>) (br' ^. ifFalse)
               _             -> bl & branches %~ (<> [br'])

-- |For each branch in a 'CondBlock', see if its 'ifFalse' branch has an
-- empty 'always' part. If that's the case we can merge the branch to the
-- parent block.
floatBranches :: forall a. (Eq a, Monoid a) => CondBlock a -> CondBlock a
floatBranches block
  = foldl' go (block & branches .~ []) (block ^. branches)
  where
    go :: CondBlock a -> CondBranch a -> CondBlock a
    go bl br
      = case br ^. ifFalse of
          Nothing -> bl & branches %~ (<> [br])
          Just bl'
            | bl' ^. always == mempty
                -> let br'  = br & ifFalse .~ Nothing
                       bl'' = floatBranches bl'
                   in
                     bl & branches %~ (<> (br' : bl'' ^. branches))
            | otherwise
                -> bl & branches %~ (<> [br])

instance (Eq a, Monoid a) => GarbageCollectable (CondBlock a) where
  garbageCollect block
    = simplify $ block & branches %~ (garbageCollect <$>)

instance Semigroup a => Simplifiable (CondBranch a) where
  simplify
    = (condition %~ simplify)
    . (ifTrue    %~ simplify)
    . (ifFalse   %~ (simplify <$>))

-- |If the 'ifTrue' branch and the 'ifFalse' branch are both empty, the
-- 'condition' becomes irrelevant so we can turn it into a constant. This
-- means @'CondBlock' a@ has to form a monoid and has to be
-- equality-comparable while doing the garbage-collection.
--
-- Why do we have separate type classes for 'simplify' and
-- 'garbageCollect'? Because the latter can only be done after forcing
-- deferred monadic computations. We cannot test if @'Monad' m => m a@ is
-- empty just because @a@ is 'Eq' and 'Monoid', without forcing the
-- computation.

instance (Eq a, Monoid a) => GarbageCollectable (CondBranch a) where
  garbageCollect br
    = let c = br ^. condition
          t = garbageCollect $ br ^. ifTrue
          f = garbageCollect <$> br ^. ifFalse
      in
        -- These comparisons regarding Maybe aren't beautiful. Can we do
        -- anything better?
        if t == mempty && (isNothing f || f == Just mempty)
        then CondBranch (Literal False) t f
        else CondBranch c t f

instance Simplifiable Condition where
  simplify c@(Literal _) = c

  simplify (Not c)
    = case simplify c of
        Literal b -> Literal (not b)
        c'        -> Not c'

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
