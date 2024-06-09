{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Extractor.Component (ComponentMeta, ComponentType(..), cType, cName, cDeps)
import Cabal2Pkg.Extractor.Conditional
  ( CondBlock, CondBranch, Condition(..), always, branches, condition, ifTrue, ifFalse )
import Cabal2Pkg.Extractor.Dependency (DepSet, exeDeps, extLibDeps, libDeps, pkgConfDeps)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep(..))
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep(..))
import Cabal2Pkg.Extractor.Dependency.Library (LibDep(..))
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep(..))
import Data.Data (gmapQl)
import Data.Generics.Aliases (GenericQ, mkQ)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Types.Flag (FlagName)
import Distribution.Types.Flag qualified as C
import GHC.Stack (HasCallStack)
import Language.BMake.AST
  ( Makefile(..), Block(..), Directive(..), (#), (.=), (.+=), blank, include
  , prettyPrintAST )
import Language.BMake.AST qualified as AST
import Lens.Micro ((&), (^.), (.~), to)


genMakefile :: HasCallStack => PackageMeta -> TL.Text
genMakefile = prettyPrintAST . genAST

anywhere :: GenericQ Bool -> GenericQ Bool
anywhere q = go
  where
    go :: GenericQ Bool
    go x
      | q x       = True
      | otherwise = gmapQl (||) False go x

genAST :: HasCallStack => PackageMeta -> Makefile
genAST meta
  = mconcat [ header
            , toolsAndConfigArgs
            , maybeUnrestrictDeps
            , maybePrefs
            , mconcat $ genComponentAST <$> comps'
            , footer
            ]
  where
    header :: Makefile
    header = Makefile
             [ blank # "$NetBSD$"
             , blank
             , "DISTNAME"   .= pure (distName meta)
             , "CATEGORIES" .= categories meta
             , blank
             , "MAINTAINER" .= pure (maintainer meta)
             , "COMMENT"    .= pure (comment meta)
             , "LICENSE"    .= pure (license meta)
             , blank
             ]

    toolsAndConfigArgs :: Makefile
    toolsAndConfigArgs
      = let section = useTools <> configArgs
        in
          if section == mempty
          then section
          else section <> Makefile [ blank ]

    -- The list of HASKELL_UNRESTRICT_DEPENDENCIES. They don't need to be
    -- conditionalised, so we gather all of them from the entire
    -- conditional tree.
    maybeUnrestrictDeps :: Makefile
    maybeUnrestrictDeps
      = if S.null (unrestrict meta)
        then mempty
        else Makefile [ "HASKELL_UNRESTRICT_DEPENDENCIES" .+= S.toList (unrestrict meta)
                      , blank
                      ]

    -- If any of the conditionals depend of variables defined in
    -- "../../mk/bsd.fast.prefs.mk", include it here.
    maybePrefs :: Makefile
    maybePrefs
      | anywhere go meta = Makefile [ include "../../mk/bsd.fast.prefs.mk"
                                    , blank
                                    ]
      | otherwise        = mempty
      where
        go :: GenericQ Bool
        go = mkQ False needsPrefs'

        needsPrefs' :: Condition -> Bool
        needsPrefs' (Literal _) = False
        needsPrefs' (Not     _) = False
        needsPrefs' (Or    _ _) = False
        needsPrefs' (And   _ _) = False
        needsPrefs' (Expr  _ b) = b

    -- The top-level USE_TOOLS. This exists only when we have just one
    -- component and at least one unconditional pkg-config dependency or an
    -- unconditional tool dependency.
    useTools :: Makefile
    useTools
      = case components meta of
          (c:[]) ->
            let exeDeps'      = addPkgConf $ c ^. cDeps . always . exeDeps
                addPkgConf xs
                  | c ^. cDeps . always . pkgConfDeps . to (not . null)
                      -- We have an unconditional dependency on a
                      -- pkg-config package. Add it to USE_TOOLS.
                      = KnownExe "pkg-config" : xs
                  | otherwise
                      = xs
            in
              genExeDepsAST exeDeps'
          _ -> mempty

    configArgs :: Makefile
    configArgs
      | M.null (flags meta) = mempty
      | otherwise
          = Makefile [ "CONFIGURE_ARGS" .+= flags' ]
      where
        flags' :: [Text]
        flags' = go <$> M.toList (flags meta)

        go :: (FlagName, Bool) -> Text
        go (flag, True ) = "-f +" <> f2t flag
        go (flag, False) = "-f -" <> f2t flag

        f2t :: FlagName -> Text
        f2t = T.pack . C.unFlagName

    -- If we only have a single component, remove unconditional tool
    -- dependencies because we move them just below the header.
    comps' :: [ComponentMeta]
    comps'
      = case components meta of
          (c:[]) ->
            let c' = c & cDeps . always . exeDeps .~ []
            in
              [c']
          cs ->
            cs

    footer :: Makefile
    footer = Makefile
             [ include "../../mk/haskell.mk"
             , include "../../mk/bsd.pkg.mk"
             ]

    genComponentAST :: HasCallStack => ComponentMeta -> Makefile
    genComponentAST
      | length (components meta) == 1 = genSingleComponentAST
      | otherwise                     = genMultiComponentAST

genSingleComponentAST :: HasCallStack => ComponentMeta -> Makefile
genSingleComponentAST c
  = genDepsAST $ c ^. cDeps

genMultiComponentAST :: HasCallStack => ComponentMeta -> Makefile
genMultiComponentAST c
  = mconcat [ header
            , genDepsAST $ c ^. cDeps
            , footer
            ]
  where
    header :: Makefile
    header
      = let ty = case c ^. cType of
                   Library    -> "lib"
                   ForeignLib -> "flib"
                   Executable -> "exe"
        in
          Makefile [ blank # ty <> ":" <> (c ^. cName) ]

    footer :: Makefile
    footer = Makefile [ blank ]

genDepsAST :: HasCallStack => CondBlock DepSet -> Makefile
genDepsAST bl
  = genDepSetAST (bl ^. always) <>
    genBranchesAST (bl ^. branches)
  where
    genBranchesAST :: HasCallStack => [CondBranch DepSet] -> Makefile
    genBranchesAST []       = mempty
    genBranchesAST (br:brs)
      = Makefile . (: []) . BDirective . DConditional $ genBranchesAST' (br :| brs)

    genBranchesAST' :: HasCallStack => NonEmpty (CondBranch DepSet) -> AST.Conditional
    genBranchesAST' (br :| brs)
      = case brs of
          []     -> clAST
          (x:xs) -> clAST <> genBranchesAST' (x :| xs)
      where
        conAST :: HasCallStack => AST.Condition
        conAST = genConditionAST $ br ^. condition

        clAST :: AST.Conditional
        clAST = AST.Conditional
                { branches = (:| []) $
                             AST.CondBranch conAST (genDepsAST $ br ^. ifTrue)
                , else_    = genDepsAST <$> br ^. ifFalse
                }

genConditionAST :: HasCallStack => Condition -> AST.Condition
genConditionAST = AST.If . go
  where
    go :: HasCallStack => Condition -> AST.LogicalExpr AST.Expr
    go c = case c of
             Literal {} -> error ("Literals should have been simplified before "
                                  <> "translating into bmake AST: " <> show c)
             Not c'     -> AST.Not (go c')
             Or  ca cb  -> flattenExpr $ AST.Or  [go ca, go cb]
             And ca cb  -> flattenExpr $ AST.And [go ca, go cb]
             Expr e _   -> AST.Expr e

-- |For each sub-expression in 'Or' or 'And', if it's also 'Or' or 'And'
-- then merge it with the parent. That is, transform @a || (b || c)@ into @a
-- || b || c@.
flattenExpr :: HasCallStack => AST.LogicalExpr a -> AST.LogicalExpr a
flattenExpr e@(AST.Not _ ) = e
flattenExpr   (AST.Or  es) = flattenOr  es
flattenExpr   (AST.And es) = flattenAnd es
flattenExpr e@(AST.Expr _) = e

flattenOr :: (HasCallStack, Foldable f) => f (AST.LogicalExpr a) -> AST.LogicalExpr a
flattenOr = AST.Or . NE.fromList . foldr go []
  where
    go e es
      = case flattenExpr e of
          AST.Not _  -> e : es
          AST.Or  es'-> NE.toList es' <> es
          AST.And _  -> e : es
          AST.Expr _ -> e : es

flattenAnd :: (HasCallStack, Foldable f) => f (AST.LogicalExpr a) -> AST.LogicalExpr a
flattenAnd = AST.And . NE.fromList . foldr go []
  where
    go e es
      = case flattenExpr e of
          AST.Not _  -> e : es
          AST.Or  _  -> e : es
          AST.And es'-> NE.toList es' <> es
          AST.Expr _ -> e : es

genDepSetAST :: DepSet -> Makefile
genDepSetAST ds
  = mconcat [ genExeDepsAST $ ds ^. exeDeps
            , mconcat $ genExtLibDepAST  <$> ds ^. extLibDeps
            , mconcat $ genLibDepAST     <$> ds ^. libDeps
            , mconcat $ genPkgConfDepAST <$> ds ^. pkgConfDeps
            ]

genExeDepsAST :: [ExeDep] -> Makefile
genExeDepsAST es
  | null known && null unknown = mempty
  | otherwise                  = Makefile [ maybeUnknown $ "USE_TOOLS" .+= known ]
  where
    known :: [Text]
    known = catMaybes $ go <$> es
      where
        go (KnownExe   {..}) = Just name
        go (UnknownExe {  }) = Nothing

    unknown :: [Text]
    unknown = catMaybes $ go <$> es
      where
        go (KnownExe   {  }) = Nothing
        go (UnknownExe {..}) = Just name

    maybeUnknown :: Block -> Block
    maybeUnknown bl
      = case unknown of
          []     -> bl
          (x:[]) -> bl # "TODO: unknown tool: " <> x
          xs     -> bl # "TODO: unknown tools: " <> T.intercalate ", " xs

genExtLibDepAST :: ExtLibDep -> Makefile
genExtLibDepAST (ExtLibDep name)
  = Makefile [ blank # "TODO: Include buildlink3.mk for lib" <> name ]

genLibDepAST :: LibDep -> Makefile
genLibDepAST (KnownLib   {..})
  = Makefile [ include $ "../../" <> pkgPath <> "/buildlink3.mk" ]
genLibDepAST (UnknownLib {..})
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]

genPkgConfDepAST :: PkgConfDep -> Makefile
genPkgConfDepAST (PkgConfDep name)
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]
