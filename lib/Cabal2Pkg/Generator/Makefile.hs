{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Generator.Makefile
  ( genMakefile
  , genComponentsAST
  ) where

import Cabal2Pkg.Extractor (PackageMeta)
import Cabal2Pkg.Extractor qualified as PM
import Cabal2Pkg.Extractor.Component (ComponentMeta, ComponentType(..), cType, cName, cDeps)
import Cabal2Pkg.Extractor.Conditional
  ( CondBlock, Conditional, CondBranch, Condition(..)
  , always, conds, branches, condElse, condition, condBlock )
import Cabal2Pkg.Extractor.Dependency (DepSet, exeDeps, extLibDeps, libDeps, pkgConfDeps)
import Cabal2Pkg.Extractor.Dependency.Executable (ExeDep(..))
import Cabal2Pkg.Extractor.Dependency.ExternalLib (ExtLibDep(..))
import Cabal2Pkg.Extractor.Dependency.Library (LibDep(..))
import Cabal2Pkg.Extractor.Dependency.PkgConfig (PkgConfDep(..))
import Cabal2Pkg.Extractor.Dependency.Version (cmpRange)
import Cabal2Pkg.Site (PackageURI(..), isFromHackage)
import Cabal2Pkg.Site.GitHub (genGitHubMasterSites)
import Cabal2Pkg.Site.GitLab (genGitLabMasterSites)
import Data.CaseInsensitive qualified as CI
import Data.Data (gmapQl)
import Data.Foldable (toList)
import Data.Generics.Aliases (GenericQ, mkQ, extQ)
import Data.Generics.Schemes (everything)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Set.Ordered (OSet)
import Data.Set.Ordered qualified as OS
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
  ( Makefile(..), Block(..), Directive(..), prettyPrintMakefile )
import Language.BMake.AST qualified as AST
import Language.BMake.AST.Plain ((#), (.=), (.+=), PlainAST, blank, include)
import Lens.Micro.Platform ((&), (^.), (%~), (.~), to)
import Network.URI (uriToString)
import Network.URI.Lens (uriPathLens)
import System.FilePath.Posix qualified as FP

data DepMethod = Build | Full
  deriving (Eq, Show)


genMakefile :: HasCallStack => PackageMeta -> TL.Text
genMakefile = prettyPrintMakefile . genAST

anywhere :: GenericQ Bool -> GenericQ Bool
anywhere q = go
  where
    go :: GenericQ Bool
    go x
      | q x       = True
      | otherwise = gmapQl (||) False go x

genAST :: HasCallStack => PackageMeta -> Makefile PlainAST
genAST pm
  = mconcat [ header
            , toolsAndConfigArgs
            , maybeUnrestrictDeps
            , maybePrefs
            , genComponentsAST pm comps'
            , footer
            ]
  where
    header :: Makefile PlainAST
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
             <> ( if PM.pkgBase pm == "hs-" <> distBase' then
                    mempty
                  else
                    let pkgName
                          | PM.pkgBase pm == CI.foldCase distBase' =
                              "${DISTNAME:tl}"
                          | otherwise =
                              "hs-${DISTNAME:tl}"
                    in
                      [ "PKGNAME" .= pure pkgName ]
                )
             <> case PM.categories pm of
                  []   -> [ "CATEGORIES" .= mempty # "TODO" ]
                  cats -> [ "CATEGORIES" .= cats ]
             <> genMasterSites pm
             <> [ blank ]
             <> case PM.owner pm of
                  Just o  -> [ "OWNER" .= pure o ]
                  Nothing ->
                    case PM.maintainer pm of
                      Just m  -> [ "MAINTAINER" .= pure m ]
                      Nothing -> mempty
             <> case PM.homepage pm of
                  _ | isFromHackage (PM.origin pm) ->
                        -- mk/haskell.mk has a good default for packages
                        -- coming from Hackage.
                        []
                  h | T.null h  ->
                        [ "HOMEPAGE" .= mempty # "TODO" ]
                    | otherwise ->
                        [ "HOMEPAGE" .= pure h ]
             <> case PM.comment pm of
                  c | T.null c ->
                        [ "COMMENT" .= mempty # "TODO: Short description of the package" ]
                    | otherwise ->
                        [ "COMMENT" .= pure (PM.comment pm) ]
             <> [ case T.uncons (PM.license pm) of
                    Just ('#', l') ->
                      "LICENSE" .= [] # T.strip l'
                    _ ->
                      "LICENSE" .= pure (PM.license pm)
                , blank
                ]

    distBase' :: Text
    distBase' = T.pack . prettyShow . PM.distBase $ pm

    distName :: Text
    distName = mconcat [ distBase'
                       , "-"
                       , T.pack . prettyShow . PM.distVersion $ pm
                       ]

    toolsAndConfigArgs :: Makefile PlainAST
    toolsAndConfigArgs
      = let section = useTools <> configArgs
        in
          if section == mempty
          then section
          else section <> Makefile [ blank ]

    -- The list of HASKELL_UNRESTRICT_DEPENDENCIES. They don't need to be
    -- conditionalised, so we gather all of them from the entire
    -- conditional tree.
    maybeUnrestrictDeps :: Makefile PlainAST
    maybeUnrestrictDeps
      | S.null depsToUnrestrict =
          mempty
      | otherwise =
          Makefile [ "HASKELL_UNRESTRICT_DEPENDENCIES" .+=
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
    maybePrefs :: Makefile PlainAST
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

    -- The top-level USE_TOOLS. This exists only when (A) we have just one
    -- component and it has at least one unconditional tool dependency, or
    -- (B) we have at least one pkg-config dependency somewhere in the
    -- conditional tree.
    useTools :: Makefile PlainAST
    useTools
      | [c] <- gcComponents . PM.components $ pm =
          let exeDeps'      = addPkgConf $ c ^. cDeps . always . exeDeps . to toList
              addPkgConf xs
                | havePkgConfDeps =
                    -- We have an unconditional dependency on a pkg-config
                    -- package. Add it to USE_TOOLS.
                    let pkgConf = KnownPkgsrcExe
                                  { name     = "pkg-config"
                                  , pkgPath  = mempty
                                  , version  = C.nullVersion
                                  , verRange = C.anyVersion
                                  }
                    in
                      pkgConf : xs
                | otherwise =
                    xs
          in genExeDepsAST exeDeps'
      | havePkgConfDeps =
          genExeDepsAST [ KnownPkgsrcExe
                          { name     = "pkg-config"
                          , pkgPath  = mempty
                          , version  = C.nullVersion
                          , verRange = C.anyVersion
                          }
                        ]
      | otherwise =
          mempty

    havePkgConfDeps :: Bool
    havePkgConfDeps = anywhere q pm
      where
        q :: GenericQ Bool
        q = mkQ False q'

        q' :: DepSet -> Bool
        q' = (^. pkgConfDeps . to (not . null))

    configArgs :: Makefile PlainAST
    configArgs
      | M.null (PM.flags pm) = mempty
      | otherwise
          = Makefile [ "CONFIGURE_ARGS" .+= flags' ]
      where
        flags' :: [Text]
        flags' = go <$> M.toList (PM.flags pm)

        go :: (FlagName, Bool) -> Text
        go (flag, True ) = "-f +" <> f2t flag
        go (flag, False) = "-f -" <> f2t flag

        f2t :: FlagName -> Text
        f2t = T.pack . C.unFlagName

    -- If we only have a single component, remove unconditional tool
    -- dependencies because we move them just below the header.
    comps' :: [ComponentMeta]
    comps' =
      case gcComponents . PM.components $ pm of
        [c] ->
          let c' = c & cDeps . always . exeDeps .~ OS.empty
          in
            [c']
        cs ->
          cs

    footer :: Makefile PlainAST
    footer = Makefile
             [ include "../../mk/haskell.mk"
             , include "../../mk/bsd.pkg.mk"
             ]

genMasterSites :: HasCallStack => PackageMeta -> [Block PlainAST]
genMasterSites pm =
  case PM.origin pm of
    HTTP httpURI ->
      let httpURI' = httpURI & uriPathLens %~ FP.dropFileName
      in
        [ "MASTER_SITES" .= [T.pack $ uriToString id httpURI' ""] ]

    File {} ->
      [ "MASTER_SITES" .= [] # "empty" ]

    GitHub dist ->
      genGitHubMasterSites (PM.pkgBase pm) (PM.distVersion pm) dist

    GitLab dist ->
      genGitLabMasterSites (PM.pkgBase pm) (PM.distVersion pm) dist

    Hackage {} ->
      [] -- mk/haskell.mk takes care of this.

-- | Generate a Makefile AST for component dependencies like
--
-- > # lib:foo
-- > .include "../../devel/hs-bar/buildlink3.mk"
-- >
-- > # exe:foo
-- > .include "../../devel/hs-baz/buildlink3.mk"
genComponentsAST :: HasCallStack => PackageMeta -> [ComponentMeta] -> Makefile PlainAST
genComponentsAST pm comps
  | [cm] <- comps = genSingleComponentAST pm cm
  | otherwise     = mconcat $ genMultiComponentAST pm <$> comps

-- |Omit components that have no visible dependencies, i.e. pkgsrc
-- dependencies, not compiler-bundled ones.
gcComponents :: HasCallStack => [ComponentMeta] -> [ComponentMeta]
gcComponents = filter p
  where
    p :: ComponentMeta -> Bool
    p = anywhere nonNull

    nonNull :: GenericQ Bool
    nonNull = mkQ False (not . nullDS)

    -- THINKME: What about conditionals that become empty after this
    -- cleanup? See also filterRunDeps in Cabal2Pkg.Generator.Buildlink3.
    nullDS :: DepSet -> Bool
    nullDS ds =
      and @[] [ ds ^. exeDeps     . to omitBundledExe . to OS.null
              , ds ^. extLibDeps                      . to OS.null
              , ds ^. libDeps     . to omitBundledLib . to OS.null
              , ds ^. pkgConfDeps                     . to OS.null
              ]

    omitBundledExe :: OSet ExeDep -> OSet ExeDep
    omitBundledExe = OS.filter q
      where
        q :: ExeDep -> Bool
        q (KnownBundledExe {}) = False
        q (KnownPkgsrcExe  {}) = True
        q (UnknownExe      {}) = True

    omitBundledLib :: OSet LibDep -> OSet LibDep
    omitBundledLib = OS.filter q
      where
        q :: LibDep -> Bool
        q (KnownBundledLib {}) = False
        q (KnownPkgsrcLib  {}) = True
        q (UnknownLib      {}) = True

genSingleComponentAST :: HasCallStack => PackageMeta -> ComponentMeta -> Makefile PlainAST
genSingleComponentAST pm cm
  = genDepsAST pm cm (cm ^. cDeps)

genMultiComponentAST :: HasCallStack => PackageMeta -> ComponentMeta -> Makefile PlainAST
genMultiComponentAST pm cm
  = mconcat [ header
            , genDepsAST pm cm (cm ^. cDeps)
            , footer
            ]
  where
    header :: Makefile PlainAST
    header =
      let f  = (<> ":" <> cm ^. cName)
          hd = case cm ^. cType of
                 CustomSetup -> "custom-setup"
                 Library     -> f "lib"
                 ForeignLib  -> f "flib"
                 Executable  -> f "exe"
      in
        Makefile [ blank # hd ]

    footer :: Makefile PlainAST
    footer = Makefile [ blank ]

genDepsAST :: HasCallStack => PackageMeta -> ComponentMeta -> CondBlock DepSet -> Makefile PlainAST
genDepsAST pm cm bl =
  genDepSetAST pm cm (bl ^. always) <>
  genConditionalsAST (bl ^. conds)
  where
    genConditionalsAST :: HasCallStack => [Conditional DepSet] -> Makefile PlainAST
    genConditionalsAST =
      Makefile . ((BDirective . DConditional . genConditionalAST) <$>)

    genConditionalAST :: HasCallStack => Conditional DepSet -> AST.Conditional PlainAST
    genConditionalAST cd =
      AST.Conditional
      { condExt      = True
      , condBranches = case L.uncons (cd ^. branches) of
                         Nothing ->
                           error $ mconcat [ "Empty conditionals should have been "
                                           , "deleted before translating into bmake AST: "
                                           , show bl
                                           ]
                         Just (br, brs) ->
                           genBranchAST br :| (genBranchAST <$> brs)
      , condElse     = AST.Else () Nothing . genDepsAST pm cm <$> cd ^. condElse
      , condEnd      = AST.EndIf () Nothing
      }

    genBranchAST :: HasCallStack => CondBranch DepSet -> AST.CondBranch PlainAST
    genBranchAST br =
      let conAST  = genConditionAST (br ^. condition)
          bodyAST = genDepsAST pm cm (br ^. condBlock)
      in
        AST.CondBranch conAST bodyAST

genConditionAST :: HasCallStack => Condition -> AST.Condition PlainAST
genConditionAST = flip (AST.If () . go) Nothing
  where
    go :: HasCallStack => Condition -> AST.LogicalExpr PlainAST (AST.Expr PlainAST)
    go c = case c of
             Literal {} -> error ("Literals should have been simplified before "
                                  <> "translating into bmake AST: " <> show c)
             Not c'     -> AST.Not () (go c')
             Or  ca cb  -> flattenExpr $ AST.Or  () [go ca, go cb]
             And ca cb  -> flattenExpr $ AST.And () [go ca, go cb]
             Expr e _   -> AST.Expr () e

-- |For each sub-expression in 'Or' or 'And', if it's also 'Or' or 'And'
-- then merge it with the parent. That is, transform @a || (b || c)@ into @a
-- || b || c@.
flattenExpr :: HasCallStack => AST.LogicalExpr PlainAST a -> AST.LogicalExpr PlainAST a
flattenExpr e@(AST.Not  _ _ ) = e
flattenExpr   (AST.Or   _ es) = flattenOr  es
flattenExpr   (AST.And  _ es) = flattenAnd es
flattenExpr e@(AST.Expr _ _ ) = e

flattenOr :: (HasCallStack, Foldable f)
          => f (AST.LogicalExpr PlainAST a)
          ->    AST.LogicalExpr PlainAST a
flattenOr = AST.Or () . NE.fromList . foldr go []
  where
    go e es
      = case flattenExpr e of
          AST.Not  _ _   -> e : es
          AST.Or   _ es' -> NE.toList es' <> es
          AST.And  _ _   -> e : es
          AST.Expr _ _   -> e : es

flattenAnd :: (HasCallStack, Foldable f)
           => f (AST.LogicalExpr PlainAST a)
           ->    AST.LogicalExpr PlainAST a
flattenAnd = AST.And () . NE.fromList . foldr go []
  where
    go e es
      = case flattenExpr e of
          AST.Not  _ _   -> e : es
          AST.Or   _ _   -> e : es
          AST.And  _ es' -> NE.toList es' <> es
          AST.Expr _ _   -> e : es

genDepSetAST :: PackageMeta -> ComponentMeta -> DepSet -> Makefile PlainAST
genDepSetAST pm cm ds =
  mconcat [ genExeDepsAST $ ds ^. exeDeps . to toList
          , mconcat $ genExtLibDepAST    meth <$> ds ^. extLibDeps  . to toList
          , mconcat $ genLibDepAST pm cm meth <$> ds ^. libDeps     . to toList
          , mconcat $ genPkgConfDepAST   meth <$> ds ^. pkgConfDeps . to toList
          ]
  where
    meth :: DepMethod
    meth | CustomSetup <- cm ^. cType = Build
         | otherwise                  = Full

genExeDepsAST :: [ExeDep] -> Makefile PlainAST
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

    maybeUnknown :: Block PlainAST -> Block PlainAST
    maybeUnknown bl
      = case unknown of
          []  -> bl
          [x] -> bl # "TODO: unknown tool: " <> x
          xs  -> bl # "TODO: unknown tools: " <> T.intercalate ", " xs

genExtLibDepAST :: DepMethod -> ExtLibDep -> Makefile PlainAST
genExtLibDepAST meth (ExtLibDep name) =
  meth' <> Makefile [ blank # "TODO: Include buildlink3.mk for package providing library " <> name ]
  where
    meth' :: Makefile PlainAST
    meth' = case meth of
              Full  -> mempty
              Build ->
                Makefile [ blank # "TODO: Set BUILDLINK_DEPMETHOD for " <> name <> " to \"build\"" ]

genLibDepAST :: PackageMeta -> ComponentMeta -> DepMethod -> LibDep -> Makefile PlainAST
genLibDepAST pm cm meth libDep =
  case libDep of
    KnownBundledLib {  } -> mempty
    KnownPkgsrcLib  {..}
      | cm ^. cType == CustomSetup && pkgBase == "gtk2hs-buildtools" ->
          -- A special case for custom-setup depending on
          -- gtk2hs-buildtools. The tool is not a library but is an
          -- executable, so the correct way to depend on it is to add it to
          -- USE_TOOLS.
          Makefile [ "USE_TOOLS" .+= [pkgBase] ]
      | cm ^. cType == Executable && pkgBase == "hs-optparse-applicative" ->
          -- A special case for executables depending on
          -- optparse-applicative. Most of the time, if not always, their
          -- command-line interface is built on top of optparse-applicative
          -- and thus support generating shell completion scripts.
          let exeDecl = if cm ^. cName == PM.pkgBase pm then
                          mempty
                        else
                          Makefile [ "OPTPARSE_APPLICATIVE_EXECUTABLES" .+= [cm ^. cName] ]
          in exeDecl <> Makefile [ include $ "../../" <> pkgPath <> "/application.mk" ]
      | otherwise ->
          let meth' = case meth of
                        Full  -> mempty
                        Build ->
                          Makefile [ AST.Value () ("BUILDLINK_DEPMETHOD." <> pkgBase) .= ["build"] ]
          in meth' <> Makefile [ include $ "../../" <> pkgPath <> "/buildlink3.mk" ]
    UnknownLib {..} ->
      let name' = T.pack . prettyShow $ name
          meth' = case meth of
                    Full  -> mempty
                    Build ->
                      Makefile [ blank # "TODO: Set BUILDLINK_DEPMETHOD for " <> name' <> " to \"build\"" ]
      in meth' <> Makefile [ blank # mconcat [ "TODO: Package \""
                                             , name'
                                             , "\" and include its buildlink3.mk"
                                             ] ]

genPkgConfDepAST :: DepMethod -> PkgConfDep -> Makefile PlainAST
genPkgConfDepAST meth (PkgConfDep name) =
  meth' <> Makefile [ blank # "TODO: Include buildlink3.mk for package providing pkg-config " <> name ]
  where
    meth' :: Makefile PlainAST
    meth' = case meth of
              Full  -> mempty
              Build ->
                Makefile [ blank # "TODO: Set BUILDLINK_DEPMETHOD for " <> name <> " to \"build\"" ]
