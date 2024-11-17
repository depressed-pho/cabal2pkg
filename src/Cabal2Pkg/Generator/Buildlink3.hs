{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator.Buildlink3
  ( genBuildlink3
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Extractor.Component
  ( ComponentMeta, ComponentType(..), cDeps, cType )
import Cabal2Pkg.Extractor.Conditional
  ( CondBlock, CondBranch, always, branches, ifTrue, ifFalse )
import Cabal2Pkg.Extractor.Dependency (DepSet(..), exeDeps)
import Cabal2Pkg.Generator.Makefile (genComponentsAST)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.MonoTraversable (omap)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version qualified as C
import GHC.Stack (HasCallStack)
import Language.BMake.AST
  ( Makefile(..), Block(..), Directive(..), (#), (.+=), (.?=), (.:=), blank
  , prettyPrintAST, unindent )
import Language.BMake.AST qualified as AST
import Lens.Micro ((^.), (.~), (%~), to)


genBuildlink3 :: HasCallStack => PackageMeta -> TL.Text
genBuildlink3 = prettyPrintAST . genAST

genAST :: HasCallStack => PackageMeta -> Makefile
genAST pm
  = mconcat [ header
            , guarded
            , footer
            ]
  where
    header :: Makefile
    header = Makefile
             [ blank # "$NetBSD$"
             , blank
             , "BUILDLINK_TREE" .+= [pkgBase pm]
             , blank
             ]

    guarded :: Makefile
    guarded = Makefile [ BDirective (DConditional guarded') ]
      where
        guarded' :: AST.Conditional
        guarded' = AST.Conditional (AST.CondBranch cond mk :| []) Nothing
                   (Just . coerce $ guardVar)

        cond :: AST.Condition
        cond = AST.If (AST.Not (AST.Expr (AST.EDefined guardVar)))

        mk :: Makefile
        mk = Makefile
             [ guardVar .:= []
             , blank
             , apiDepends
             , abiDepends
             , pkgsrcDir
             ]
             <> omap unindent (genComponentsAST pm comps')

        guardVar :: AST.Variable
        guardVar = AST.Variable . (<> "_BUILDLINK3_MK") . T.map go . T.toUpper . pkgBase $ pm
          where
            go :: Char -> Char
            go '-' = '_'
            go c   = c

        apiDepends :: Block
        apiDepends = var .+= [pat]
          where
            var = AST.Variable $ "BUILDLINK_API_DEPENDS." <> pkgBase pm
            pat = pkgBase pm <> ">=" <> T.pack (prettyShow ver)
            ver = C.alterVersion f . distVersion $ pm
            -- Drop the forth component of the version. This is valid as
            -- long as the package adopts PVP: https://pvp.haskell.org/
            f :: [Int] -> [Int]
            f = take 3

        abiDepends :: Block
        abiDepends = var .+= [pat]
          where
            var = AST.Variable $ "BUILDLINK_ABI_DEPENDS." <> pkgBase pm
            -- FIXME: We should take PKGREVISION into account in "cabal2pkg update".
            pat = pkgBase pm <> "==" <> T.pack (prettyShow $ distVersion pm)

        pkgsrcDir :: Block
        pkgsrcDir = var .?= [dir]
          where
            var = AST.Variable $ "BUILDLINK_PKGSRCDIR." <> pkgBase pm
            dir = "../../" <> pkgPath pm

    -- Since this is a buildlink3.mk, we only need to generate dependency
    -- lists for Haskell library or foreign library components. Components
    -- having no runtime dependencies should also be omitted.
    comps' :: [ComponentMeta]
    comps' = filter (^. (cDeps . to hasDeps))
             . fmap (cDeps %~ filterRunDeps)
             . filter isLib
             . components $ pm
      where
        isLib :: ComponentMeta -> Bool
        isLib cm
          = case cm ^. cType of
              Library    -> True
              ForeignLib -> True
              Executable -> False

    footer :: Makefile
    footer = Makefile
             [ blank
             , "BUILDLINK_TREE" .+= ["-" <> pkgBase pm]
             ]

hasDeps :: (Eq a, Monoid a) => CondBlock a -> Bool
hasDeps bl
  = bl ^. always . to (/= mempty) ||
    bl ^. branches . to (any go)
  where
    go br
      = br ^. ifTrue . to hasDeps ||
        br ^. ifFalse . to (any hasDeps)

-- THINKME: Maybe we should apply (floatBranches . garbageCollect) after
-- this?
filterRunDeps :: CondBlock DepSet -> CondBlock DepSet
filterRunDeps
  = (always %~ exeDeps .~ [])
  . (branches %~ (go <$>))
  where
    go :: CondBranch DepSet -> CondBranch DepSet
    go = (ifTrue  %~ filterRunDeps)
       . (ifFalse %~ (filterRunDeps <$>))
