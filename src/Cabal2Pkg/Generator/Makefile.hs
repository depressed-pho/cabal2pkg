{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  , genComponentsAST
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
import Cabal2Pkg.Extractor.Dependency.Version (cmpRange)
import Cabal2Pkg.Site (PackageURI(..), isFromHackage)
import Cabal2Pkg.Site.GitHub (genGitHubMasterSites)
import Cabal2Pkg.Site.GitLab (genGitLabMasterSites)
import Data.Data (gmapQl)
import Data.Foldable (toList)
import Data.Generics.Aliases (GenericQ, mkQ, extQ)
import Data.Generics.Schemes (everything)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Set.Ordered qualified as OS
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Distribution.Pretty (prettyShow)
import Distribution.Types.Flag (FlagName)
import Distribution.Types.Flag qualified as C
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version qualified as C
import Distribution.Types.VersionRange qualified as C
import GHC.Stack (HasCallStack)
import Language.BMake.AST
  ( Makefile(..), Block(..), Directive(..), (#), (.=), (.+=), blank, include
  , prettyPrintAST )
import Language.BMake.AST qualified as AST
import Lens.Micro.Platform ((&), (^.), (%~), (.~), to)
import Network.URI (uriToString)
import Network.URI.Lens (uriPathLens)
import System.FilePath.Posix qualified as FP


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
genAST pm
  = mconcat [ header
            , toolsAndConfigArgs
            , maybeUnrestrictDeps
            , maybePrefs
            , genComponentsAST pm comps'
            , footer
            ]
  where
    header :: Makefile
    header = Makefile
             $  [ blank # "$NetBSD$"
                , blank
                , "DISTNAME"   .= pure distName
                ]
             -- mk/haskell.mk provides a default definition of PKGNAME as
             -- hs-${DISTNAME}. We should have defined it as
             -- hs-${DISTNAME:tl} but it's too late. Changing it at this
             -- point would cause a major breakage. So we need to
             -- explicitly define it here whenever pkgBase isn't equal to
             -- hs-{distBase}.
             <> (if pkgBase pm == "hs-" <> distBase' then
                   mempty
                 else
                   [ "PKGNAME" .= pure (if pkgBase pm == distBase' then
                                          "${DISTNAME}"
                                        else
                                          "${DISTNAME:tl}")
                   ])
             <> case categories pm of
                  []   -> [ "CATEGORIES" .= mempty # "TODO" ]
                  cats -> [ "CATEGORIES" .= cats ]
             <> genMasterSites pm
             <> [ blank ]
             <> case owner pm of
                  Just o  -> [ "OWNER" .= pure o ]
                  Nothing ->
                    case maintainer pm of
                      Just m  -> [ "MAINTAINER" .= pure m ]
                      Nothing -> mempty
             <> case homepage pm of
                  _ | isFromHackage (origin pm) ->
                        -- mk/haskell.mk has a good default for packages
                        -- coming from Hackage.
                        []
                  h | T.null h  ->
                        [ "HOMEPAGE" .= mempty # "TODO" ]
                    | otherwise ->
                        [ "HOMEPAGE" .= pure h ]
             <> case comment pm of
                  c | T.null c ->
                        [ "COMMENT" .= mempty # "TODO: Short description of the package" ]
                    | otherwise ->
                        [ "COMMENT" .= pure (comment pm) ]
             <> [ "LICENSE" .= pure (license pm)
                , blank
                ]

    distBase' :: Text
    distBase' = T.pack . prettyShow . distBase $ pm

    distName :: Text
    distName = mconcat [ distBase'
                       , "-"
                       , T.pack . prettyShow . distVersion $ pm
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
      = if S.null depsToUnrestrict
        then mempty
        else Makefile [ "HASKELL_UNRESTRICT_DEPENDENCIES" .+=
                        T.pack . prettyShow <$> S.toList depsToUnrestrict
                      , blank
                      ]
      where
        depsToUnrestrict :: Set PackageName
        depsToUnrestrict = everything (<>) go pm

        go :: GenericQ (Set PackageName)
        go = mkQ mempty exeDepU `extQ` libDepU

        exeDepU :: ExeDep -> Set PackageName
        exeDepU (KnownBundledExe {..}) = if cmpRange version verRange == Just GT
                                         then S.singleton name
                                         else mempty
        exeDepU (KnownPkgsrcExe  {..}) = if cmpRange version verRange == Just GT
                                         then S.singleton name
                                         else mempty
        exeDepU (UnknownExe      {  }) = mempty

        libDepU :: LibDep -> Set PackageName
        libDepU (KnownBundledLib {..}) = if cmpRange version verRange == Just GT
                                         then S.singleton name
                                         else mempty
        libDepU (KnownPkgsrcLib  {..}) = if cmpRange version verRange == Just GT
                                         then S.singleton name
                                         else mempty
        libDepU (UnknownLib      {  }) = mempty

    -- If any of the conditionals depend of variables defined in
    -- "../../mk/bsd.fast.prefs.mk", include it here.
    maybePrefs :: Makefile
    maybePrefs
      | anywhere go pm = Makefile [ include "../../mk/bsd.fast.prefs.mk"
                                  , blank
                                  ]
      | otherwise      = mempty
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
    -- FIXME: What if we had pkg-config deps in other cases?
    useTools :: Makefile
    useTools
      = case components pm of
          [c] ->
            let exeDeps'      = addPkgConf $ c ^. cDeps . always . exeDeps . to toList
                addPkgConf xs
                  | c ^. cDeps . always . pkgConfDeps . to (not . null) =
                      -- We have an unconditional dependency on a
                      -- pkg-config package. Add it to USE_TOOLS.
                      let pkgConf = KnownPkgsrcExe
                                    { name     = "pkg-config"
                                    , pkgPath  = mempty
                                    , version  = C.nullVersion
                                    , verRange = C.anyVersion
                                    }
                      in
                        pkgConf : xs
                  | otherwise
                      = xs
            in
              genExeDepsAST exeDeps'
          _ -> mempty

    configArgs :: Makefile
    configArgs
      | M.null (flags pm) = mempty
      | otherwise
          = Makefile [ "CONFIGURE_ARGS" .+= flags' ]
      where
        flags' :: [Text]
        flags' = go <$> M.toList (flags pm)

        go :: (FlagName, Bool) -> Text
        go (flag, True ) = "-f +" <> f2t flag
        go (flag, False) = "-f -" <> f2t flag

        f2t :: FlagName -> Text
        f2t = T.pack . C.unFlagName

    -- If we only have a single component, remove unconditional tool
    -- dependencies because we move them just below the header.
    comps' :: [ComponentMeta]
    comps'
      = case components pm of
          [c] ->
            let c' = c & cDeps . always . exeDeps .~ OS.empty
            in
              [c']
          cs ->
            cs

    footer :: Makefile
    footer = Makefile
             [ include "../../mk/haskell.mk"
             , include "../../mk/bsd.pkg.mk"
             ]

genMasterSites :: HasCallStack => PackageMeta -> [Block]
genMasterSites pm =
  case origin pm of
    HTTP httpURI ->
      let httpURI' = httpURI & uriPathLens %~ FP.dropFileName
      in
        [ "MASTER_SITES" .= [T.pack $ uriToString id httpURI' ""] ]

    File {} ->
      [ "MASTER_SITES" .= [] # "empty" ]

    GitHub dist ->
      genGitHubMasterSites (pkgBase pm) (distVersion pm) dist

    GitLab dist ->
      genGitLabMasterSites (pkgBase pm) (distVersion pm) dist

    Hackage {} ->
      [] -- mk/haskell.mk takes care of this.

-- | Generate a Makefile AST for component dependencies like
--
-- > # lib:foo
-- > .include "../../devel/hs-bar/buildlink3.mk"
-- >
-- > # exe:foo
-- > .include "../../devel/hs-baz/buildlink3.mk"
genComponentsAST :: HasCallStack => PackageMeta -> [ComponentMeta] -> Makefile
genComponentsAST pm comps
  = case comps of
      [cm] -> genSingleComponentAST pm cm
      _    -> mconcat $ genMultiComponentAST pm <$> comps

genSingleComponentAST :: HasCallStack => PackageMeta -> ComponentMeta -> Makefile
genSingleComponentAST pm cm
  = genDepsAST pm cm (cm ^. cDeps)

genMultiComponentAST :: HasCallStack => PackageMeta -> ComponentMeta -> Makefile
genMultiComponentAST pm cm
  = mconcat [ header
            , genDepsAST pm cm (cm ^. cDeps)
            , footer
            ]
  where
    header :: Makefile
    header
      = let ty = case cm ^. cType of
                   Library    -> "lib"
                   ForeignLib -> "flib"
                   Executable -> "exe"
        in
          Makefile [ blank # ty <> ":" <> cm ^. cName ]

    footer :: Makefile
    footer = Makefile [ blank ]

genDepsAST :: HasCallStack => PackageMeta -> ComponentMeta -> CondBlock DepSet -> Makefile
genDepsAST pm cm bl
  = genDepSetAST pm cm (bl ^. always) <>
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
                { branches   = (:| []) $
                               AST.CondBranch conAST (genDepsAST pm cm (br ^. ifTrue))
                , else_      = genDepsAST pm cm <$> br ^. ifFalse
                , endComment = Nothing
                , indent     = True
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

genDepSetAST :: PackageMeta -> ComponentMeta -> DepSet -> Makefile
genDepSetAST pm cm ds
  = mconcat [ genExeDepsAST $ ds ^. exeDeps . to toList
            , mconcat $ genExtLibDepAST    <$> ds ^. extLibDeps  . to toList
            , mconcat $ genLibDepAST pm cm <$> ds ^. libDeps     . to toList
            , mconcat $ genPkgConfDepAST   <$> ds ^. pkgConfDeps . to toList
            ]

genExeDepsAST :: [ExeDep] -> Makefile
genExeDepsAST es
  | null known && null unknown = mempty
  | otherwise                  = Makefile [ maybeUnknown $ "USE_TOOLS" .+= known ]
  where
    known :: [Text]
    known = mapMaybe go es
      where
        go (KnownBundledExe {  }) = Nothing
        go (KnownPkgsrcExe  {..}) = Just . T.pack . prettyShow $ name
        go (UnknownExe      {  }) = Nothing

    unknown :: [Text]
    unknown = mapMaybe go es
      where
        go (KnownBundledExe {  }) = Nothing
        go (KnownPkgsrcExe  {  }) = Nothing
        go (UnknownExe      {..}) = Just . T.pack . prettyShow $ name

    maybeUnknown :: Block -> Block
    maybeUnknown bl
      = case unknown of
          []  -> bl
          [x] -> bl # "TODO: unknown tool: " <> x
          xs  -> bl # "TODO: unknown tools: " <> T.intercalate ", " xs

genExtLibDepAST :: ExtLibDep -> Makefile
genExtLibDepAST (ExtLibDep name)
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]

genLibDepAST :: PackageMeta -> ComponentMeta -> LibDep -> Makefile
genLibDepAST pm cm libDep =
  case libDep of
    KnownBundledLib {  } -> mempty
    KnownPkgsrcLib  {..}
      | cm ^. cType == Executable &&
        pkgPath == "devel/hs-optparse-applicative" ->
          -- A special case for executables depending on
          -- optparse-applicative. Most of the time, if not always, their
          -- command-line interface is built on top of optparse-applicative
          -- and thus support generating shell completion scripts.
          let exeDecl = if cm ^. cName == (T.pack . prettyShow . distBase) pm then
                          mempty
                        else
                          Makefile [ "OPTPARSE_APPLICATIVE_EXECUTABLES" .+= [cm ^. cName] ]
          in
            exeDecl <> Makefile [ include $ "../../" <> pkgPath <> "/application.mk" ]
      | otherwise ->
          Makefile [ include $ "../../" <> pkgPath <> "/buildlink3.mk" ]
    UnknownLib {..} ->
      Makefile [ blank # mconcat [ "TODO: Package \""
                                 , T.pack . prettyShow $ name
                                 , "\" and include its buildlink3.mk"
                                 ] ]

genPkgConfDepAST :: PkgConfDep -> Makefile
genPkgConfDepAST (PkgConfDep name)
  = Makefile [ blank # "TODO: Include buildlink3.mk for " <> name ]
