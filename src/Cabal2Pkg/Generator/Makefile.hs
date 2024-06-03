{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Extractor.Component (ComponentMeta, ComponentType(..), cType, cName, cDeps)
import Cabal2Pkg.Extractor.Conditional (CondBlock, always)
import Cabal2Pkg.Extractor.Dependency (DepSet, exeDeps, extLibDeps, libDeps, pkgConfDeps)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep(..))
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep(..))
import Cabal2Pkg.Extractor.Dependency.Library (LibDep(..))
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep(..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Language.BMake.AST
  ( Makefile(..), Block, (#), (.=), (.+=), blank, include, prettyPrintAST )
import Lens.Micro ((&), (^.), (.~), to)


genMakefile :: PackageMeta -> TL.Text
genMakefile = prettyPrintAST . genAST


genAST :: PackageMeta -> Makefile
genAST meta
  = mconcat [ header
            , toolsAndConfigArgs
            , mconcat $ genComponentAST <$> comps1
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

    -- FIXME: Don't forget about needsUnrestricting
    -- FIXME: And CONFIGURE_ARGS

    toolsAndConfigArgs :: Makefile
    toolsAndConfigArgs
      = let section = useTools -- <> configArgs
        in
          if section == mempty
          then section
          else section <> Makefile [ blank ]

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
                      = KnownExe "pkg-config" False : xs
                  | otherwise
                      = xs
            in
              genExeDepsAST exeDeps'
          _ -> mempty

    -- If we only have a single component, remove unconditional tool
    -- dependencies because we move them just below the header.
    comps1 :: [ComponentMeta]
    comps1
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

    genComponentAST :: ComponentMeta -> Makefile
    genComponentAST
      | length (components meta) == 1 = genSingleComponentAST
      | otherwise                     = genMultiComponentAST

genSingleComponentAST :: ComponentMeta -> Makefile
genSingleComponentAST c
  = genDepsAST $ c ^. cDeps

genMultiComponentAST :: ComponentMeta -> Makefile
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

genDepsAST :: CondBlock DepSet -> Makefile
genDepsAST b
  = genDepSetAST (b ^. always) -- FIXME: branches

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
        go (BundledExe {  }) = Nothing
        go (KnownExe   {..}) = Just name
        go (UnknownExe {  }) = Nothing

    unknown :: [Text]
    unknown = catMaybes $ go <$> es
      where
        go (BundledExe {  }) = Nothing
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
genLibDepAST (BundledLib {  }) = mempty
genLibDepAST (KnownLib   {..})
  = Makefile [ include $ "../../" <> pkgPath <> "/buildlink3.mk" ]
genLibDepAST (UnknownLib {..})
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]

genPkgConfDepAST :: PkgConfDep -> Makefile
genPkgConfDepAST (PkgConfDep name)
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]