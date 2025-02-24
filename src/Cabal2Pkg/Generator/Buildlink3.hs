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
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set.Ordered qualified as OS
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version qualified as C
import GHC.Stack (HasCallStack)
import Language.BMake.AST
  ( Makefile(..), Block(..), Directive(..), prettyPrintMakefile )
import Language.BMake.AST qualified as AST
import Language.BMake.AST.Plain ((#), (.+=), (.?=), (.:=), PlainAST, blank)
import Lens.Micro.Platform ((^.), (.~), (%~), to)


genBuildlink3 :: HasCallStack => PackageMeta -> TL.Text
genBuildlink3 = prettyPrintMakefile . genAST

genAST :: HasCallStack => PackageMeta -> Makefile PlainAST
genAST pm =
  mconcat [ header
          , guarded
          , footer
          ]
  where
    header :: Makefile PlainAST
    header = Makefile
             [ blank # "$NetBSD$"
             , blank
             , "BUILDLINK_TREE" .+= [pkgBase pm]
             , blank
             ]

    guarded :: Makefile PlainAST
    guarded = Makefile [ BDirective (DConditional guarded') ]
      where
        guarded' :: AST.Conditional PlainAST
        guarded' = AST.Conditional
                   { AST.branches   = AST.CondBranch cond mk :| []
                   , AST.elseBranch = Nothing
                   , AST.endComment = case guardVar of
                                        AST.Value _ name ->
                                          Just $ AST.Comment () name
                   , AST.indent     = False
                   }

        cond :: AST.Condition PlainAST
        cond = AST.If () (AST.Not () (AST.Expr () (AST.EDefined guardVar))) Nothing

        mk :: Makefile PlainAST
        mk = Makefile [ guardVar .:= []
                      , blank
                      , apiDepends
                      , abiDepends
                      , pkgsrcDir
                      ]
             <> case comps' of
                  [] -> mempty
                  _  -> Makefile $ pure blank
             <> genComponentsAST pm comps'

        guardVar :: AST.Value PlainAST
        guardVar = AST.Value ()
                 . (<> "_BUILDLINK3_MK")
                 . T.map go
                 . T.toUpper
                 . pkgBase
                 $ pm
          where
            go :: Char -> Char
            go '-' = '_'
            go c   = c

        apiDepends :: Block PlainAST
        apiDepends = var .+= [pat]
          where
            var = AST.Value () $ "BUILDLINK_API_DEPENDS." <> pkgBase pm
            pat = pkgBase pm <> ">=" <> T.pack (prettyShow ver)
            ver = C.alterVersion f . distVersion $ pm
            -- Drop the forth component of the version. This is valid as
            -- long as the package adopts PVP: https://pvp.haskell.org/
            f :: [Int] -> [Int]
            f = take 3

        abiDepends :: Block PlainAST
        abiDepends = var .+= [pat]
          where
            var = AST.Value () $ "BUILDLINK_ABI_DEPENDS." <> pkgBase pm
            pat = pkgBase pm <> ">=" <> T.pack (prettyShow $ distVersion pm)

        pkgsrcDir :: Block PlainAST
        pkgsrcDir = var .?= [dir]
          where
            var = AST.Value () $ "BUILDLINK_PKGSRCDIR." <> pkgBase pm
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

    footer :: Makefile PlainAST
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
  = (always %~ exeDeps .~ OS.empty)
  . (branches %~ (go <$>))
  where
    go :: CondBranch DepSet -> CondBranch DepSet
    go = (ifTrue  %~ filterRunDeps)
       . (ifFalse %~ (filterRunDeps <$>))
