{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator.Buildlink3
  ( genBuildlink3
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version qualified as C
import GHC.Stack (HasCallStack)
import Language.BMake.AST
  ( Makefile(..), Block(..), Directive(..), (#), (.+=), (.?=), (.:=), blank
  , include, prettyPrintAST )
import Language.BMake.AST qualified as AST


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

    footer :: Makefile
    footer = Makefile
             [ blank
             , "BUILDLINK_TREE" .+= ["-" <> pkgBase pm]
             ]
