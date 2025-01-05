{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Site.GitLab
  ( -- * URI
    GitLabDist(..)
  , parseGitLabDist
  , reconstructGitLabDist
  , renderGitLabDist

    -- * Makefile
  , genGitLabMasterSites
  )
  where

import Cabal2Pkg.CmdLine (CLI, gitLabURI)
import Cabal2Pkg.Site.Common (isIn)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Data.Data (Data)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import Language.BMake.AST ((.=), Block(..))
import Lens.Micro.Platform ((%~))
import Network.URI (URI, pathSegments)
import Network.URI.Lens (uriPathLens)
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix qualified as OP


data GitLabDist =
    Tag
    { glAccount  :: !Text
    , glProject  :: !Text
    , glTag      :: !Text
    }
  | Release
    { glAccount  :: !Text
    , glProject  :: !Text
    , glRelease  :: !Text
    , glDistName :: !Text
    }
  deriving (Data, Eq, Show)

-- |Try to parse a GitLab archive URI. Return 'Nothing' if it cannot be parsed.
parseGitLabDist :: URI -> CLI (Maybe GitLabDist)
parseGitLabDist uri = go <$> gitLabURI
  where
    go :: URI -> Maybe GitLabDist
    go gitLab =
      do unless (uri `isIn` gitLab) $
           fail "clearly not a GitLab URI"
         let nDrop = L.length (pathSegments gitLab)
             segs  = pathSegments uri
         case L.drop nDrop segs of
           [account, project, "-", "archive", tag, file] ->
             do distName <- FP.stripExtension ".tar.gz" file
                pure $ guess (T.pack account)
                             (T.pack project)
                             (T.pack tag)
                             (T.pack distName)
           [acc0, acc1, project, "-", "archive", tag, file] ->
             do distName <- FP.stripExtension ".tar.gz" file
                pure $ guess (T.pack $ FP.joinPath [acc0, acc1])
                             (T.pack project)
                             (T.pack tag)
                             (T.pack distName)
           _ -> fail "unknown GitLab URI"

    -- This is just a heuristic. We can never be sure if this is tag-based
    -- or release-based because of the ambiguity of GitLab URI. Let the
    -- user correct it if we guessed wrong.
    guess :: Text -> Text -> Text -> Text -> GitLabDist
    guess account project tag distName
      | distName == project <> "-" <> tag =
          Tag { glAccount = account
              , glProject = project
              , glTag     = tag
              }
      | otherwise =
          Release { glAccount  = account
                  , glProject  = project
                  , glRelease  = tag
                  , glDistName = distName
                  }

-- |Try to reconstruct a 'GitLabDist' from an existing pkgsrc
-- package. Return 'Nothing' if it's not from GitLab.
reconstructGitLabDist :: Package CLI -> CLI (Maybe GitLabDist)
reconstructGitLabDist pkg =
  runMaybeT $
  do gitlab  <- lift gitLabURI
     (uri:_) <- lift $ SrcDb.masterSites pkg
     unless (uri `isIn` gitlab) $
       fail "clearly not from GitLab"
     let nDrop = L.length (pathSegments gitlab)
         segs  = pathSegments uri
     account <- case L.drop nDrop segs of
                  [acc]        -> pure . T.pack $ acc
                  [acc0, acc1] -> pure . T.pack $ FP.joinPath [acc0, acc1]
                  _            -> fail "invalid account"
     project <- hoistMaybe =<< lift (SrcDb.gitLabProject pkg)
     typ     <- hoistMaybe =<< lift (SrcDb.gitLabType    pkg)
     case typ of
       SrcDb.GitLabTag ->
         do tag <- hoistMaybe =<< lift (SrcDb.gitLabTag pkg)
            pure $ Tag { glAccount = account
                       , glProject = project
                       , glTag     = tag
                       }
       SrcDb.GitLabRelease ->
         do release  <- hoistMaybe =<< lift (SrcDb.gitLabRelease pkg)
            distName <- lift $ (T.pack <$>) . OP.decodeUtf =<< SrcDb.distName pkg
            pure $ Release { glAccount  = account
                           , glProject  = project
                           , glRelease  = release
                           , glDistName = distName
                           }

renderGitLabDist :: GitLabDist -> CLI URI
renderGitLabDist (Tag {..}) =
  (uriPathLens %~ f) <$> gitLabURI
  where
    f base = FP.joinPath [ base
                         , T.unpack glAccount
                         , T.unpack glProject
                         , "-"
                         , "archive"
                         , T.unpack glTag
                         , T.unpack (glProject <> "-" <> glTag) <> ".tar.gz"
                         ]
renderGitLabDist (Release {..}) =
  (uriPathLens %~ f) <$> gitLabURI
  where
    f base = FP.joinPath [ base
                         , T.unpack glAccount
                         , T.unpack glProject
                         , "-"
                         , "archive"
                         , T.unpack glRelease
                         , T.unpack glDistName <> ".tar.gz"
                         ]

-- |See pkgsrc @mk/fetch/gitlab.mk@
genGitLabMasterSites :: Text -> Version -> GitLabDist -> [Block]
genGitLabMasterSites pkgBase ver dist =
  [ "MASTER_SITES" .= ["${MASTER_SITE_GITLAB:=" <> glAccount dist <> "/}"] ]
  <> [ "GITLAB_PROJECT" .= [glProject dist]
     | glProject dist /= pkgBase
     ]
  <> case dist of
       Tag {..}
         | glTag == ver' -> [] -- Use the default value.
         | otherwise     -> [ "GITLAB_TAG" .= [T.replace ver' "${PKGVERSION_NOREV}" glTag] ]
       Release {} ->
         [ "GITLAB_RELEASE" .= [glRelease dist] ]
  where
    ver' :: Text
    ver' = T.pack . prettyShow $ ver
