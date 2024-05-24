{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cabal2Pkg.Extractor.Conditional
  ( Environment(..)
  , CondBlock(..)
  , extractCondBlock
  ) where

import Cabal2Pkg.CmdLine (FlagMap)
import Data.Aeson ((.=), ToJSON(..), Value, object)
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


data Environment = Environment
  { flags      :: !FlagMap
  , ghcVersion :: !Version
  }
  deriving Show

data CondBlock a = CondBlock
  { always    :: !a
  , branches  :: ![CondBranch a]
  }
  deriving (Generic, Show)
  deriving ToJSON via Generically (CondBlock a)


data CondBranch a = CondBranch
  { condition  :: !Condition
  , ifTrue     :: !(CondBlock a)
  , ifFalse    :: !(Maybe (CondBlock a))
  }
  deriving (Generic, Show)
  deriving ToJSON via Generically (CondBranch a)


-- |An intermediate data type for Cabal conditions.
data Condition
  = Literal !Bool
  | Not !Condition
  | Or  !Condition !Condition
  | And !Condition !Condition
  | Expr
    { expression :: !Text
    , needsPrefs :: !Bool
    }
  deriving Show

instance ToJSON Condition where
  toJSON :: Condition -> Value
  toJSON (Literal b) = object [ "literal"    .= b          ]
  toJSON (Not c    ) = object [ "not"        .= c          ]
  toJSON (Or  ca cb) = object [ "or"         .= [toJSON ca, toJSON cb] ]
  toJSON (And ca cb) = object [ "and"        .= [toJSON ca, toJSON cb] ]
  toJSON (Expr {..}) = object [ "expression" .= expression
                              , "needsPrefs" .= needsPrefs ]


-- |Conditionals in the Cabal AST is fucking irritatingly
-- incomprehensible. I simply cannot get the point of the type variable @c@
-- in @CondTree v c a@. Isn't the fucking condTreeConstraints already
-- contained in @condTreeData :: a@? To confuse people trying to work with
-- the fucking AST?
extractCondBlock :: forall m a a' _c c' .
                    (HasCallStack, Monad m)
                 => (a -> m c')
                 -> (a -> CondBlock c' -> m a')
                 -> Environment
                 -> C.CondTree C.ConfVar _c a
                 -> m a'
extractCondBlock extractContent extractOuter env = go
  where
    go :: HasCallStack => C.CondTree C.ConfVar _c a -> m a'
    go tree
      = do block <- mkBlock tree
           extractOuter (C.condTreeData tree) block

    mkBlock :: HasCallStack => C.CondTree C.ConfVar _c a -> m (CondBlock c')
    mkBlock tree
      = do c  <- extractContent (C.condTreeData tree)
           bs <- traverse extractBranch (C.condTreeComponents tree)
           pure CondBlock
             { always   = c
             , branches = bs
             }

    extractBranch :: HasCallStack => C.CondBranch C.ConfVar _c a -> m (CondBranch c')
    extractBranch branch
      = do ifT <- mkBlock (C.condBranchIfTrue branch)
           ifE <- traverse mkBlock (C.condBranchIfFalse branch)
           pure CondBranch
             { condition = extractCondition (C.condBranchCondition branch)
             , ifTrue    = ifT
             , ifFalse   = ifE
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
          Nothing -> error (show env)
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
      C.OtherOS _ -> Literal False
  where
    c :: Text -> Condition
    c osName
      = Expr
        { expression = "${OPSYS} == \"" <> osName <> "\""
        , needsPrefs = True
        }

extractArchCond :: C.Arch -> Condition
extractArchCond arch
  = case arch of
      C.I386        -> c  "i386"
      C.X86_64      -> c  "x86_64"
      C.PPC         -> c  "powerpc"
      C.PPC64       -> c' "!empty(MACHINE_ARCH:Mpowerpc64*)"
      C.Sparc       -> c  "sparc"
      C.Arm         -> c' "!empty(MACHINE_ARCH:M*arm*)"
      C.AArch64     -> c  "aarch64"
      C.Mips        -> c' "!empty(MACHINE_ARCH:Mmips*)"
      C.SH          -> c' "!empty(MACHINE_ARCH:Msh3*)"
      C.IA64        -> c  "ia64"
      C.S390        -> Literal False -- not supported by pkgsrc
      C.S390X       -> Literal False
      C.Alpha       -> c  "alpha"
      C.Hppa        -> c  "hppa"
      C.Rs6000      -> Literal False
      C.M68k        -> c  "m68k"
      C.Vax         -> c  "vax"
      C.JavaScript  -> Literal False
      C.Wasm32      -> Literal False
      C.OtherArch _ -> Literal False
  where
    c :: Text -> Condition
    c archName
      = c' $ "${MACHINE_ARCH} == \"" <> archName <> "\""

    c' :: Text -> Condition
    c' expr
      = Expr
        { expression = expr
        , needsPrefs = True
        }
