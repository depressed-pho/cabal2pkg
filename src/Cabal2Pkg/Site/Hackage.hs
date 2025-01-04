{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- |Hackage recommends API users to use the hackage-security library
-- instead of directly accessing it. However, hackage-security is designed
-- to build a clone of the entire database as a local cache and update it
-- from time to time. That doesn't really suit well to our use case. So we
-- directly use its REST API.
module Cabal2Pkg.Site.Hackage
  ( -- * URI
    HackageDist(..)
  , parseHackageDist
  , reconstructHackageDist
  , renderHackageDist

    -- * API
  , PackageStatus(..)
  , AvailableVersions(..)
  , latestPreferred

    -- ** I/O
  , fetchAvailableVersions
  , fetchCabal
  , fetchChangeLog
  ) where

import Cabal2Pkg.CmdLine (CLI, info, fatal, hackageURI)
import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.Site.Common (isIn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as ATH
import Data.Aeson.Types (FromJSON(..), withObject)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Conduit ((.|), ConduitT, yield)
import Data.Conduit.Combinators (sinkLazy)
import Data.Data (Data)
import Data.Foldable (find, toList)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Database.Pkgsrc.SrcDb (Package)
import Database.Pkgsrc.SrcDb qualified as SrcDb
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier(pkgName, pkgVersion))
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import GHC.Stack (HasCallStack)
import Lens.Micro ((&), (%~))
import Network.HTTP.Media qualified as MT
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpJSON, httpSource, parseRequest )
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful, notFound404 )
import Network.URI (URI(..), pathSegments, uriToString)
import Network.URI.Lens (uriPathLens)
import Prettyprinter qualified as PP
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OP


data HackageDist = HackageDist !PackageName !(Maybe Version)
  deriving (Data, Eq, Show)

-- |Try to parse a package URI. Return 'Nothing' If it's not from Hackage.
parseHackageDist :: Maybe PackageName -- ^Context, used for the @update@ command
                 -> URI
                 -> CLI (Maybe HackageDist)
parseHackageDist ctx uri =
  case uriScheme uri of
    "http:"  -> parseHTTPURI uri
    "https:" -> parseHTTPURI uri
    ""       -> parseHackageURI ctx uri
    _        -> pure Nothing

parseHTTPURI :: URI -> CLI (Maybe HackageDist)
parseHTTPURI uri = go <$> hackageURI
  where
    go :: URI -> Maybe HackageDist
    go hackage =
      do unless (uri `isIn` hackage) $
           fail "clearly not a hackage URI"
         -- The first segment after "package" should be
         -- "{PACKAGE}-{VERSION}". The second one should be
         -- "{PACKAGE}-{VERSION}.tar.gz".
         [s1, s2] <- let nDrop = L.length (pathSegments hackage)
                         segs  = pathSegments uri
                     in
                       pure $ L.drop nDrop segs
         pkgId    <- listToMaybe . toList $ eitherParsec s1
         name     <- FP.stripExtension ".tar.gz" s2
         pkgId'   <- listToMaybe . toList $ eitherParsec name
         unless (pkgId == pkgId') $
           fail "package IDs mismatch"
         pure $ HackageDist (pkgName pkgId) (Just $ pkgVersion pkgId)

parseHackageURI :: MonadThrow m => Maybe PackageName -> URI -> m (Maybe HackageDist)
parseHackageURI Nothing (uriPath -> path) =
  case eitherParsec path of
    Right pkgId ->
      -- It's a full package ID, i.e. NAME-VERSION
      pure . Just $ HackageDist (pkgName pkgId) (Just $ pkgVersion pkgId)
    Left _ ->
      case eitherParsec path of
        Right name ->
          -- It's a package name without version
          pure . Just $ HackageDist name Nothing
        Left _ ->
          pure Nothing
parseHackageURI (Just name) (uriPath -> path) =
  case eitherParsec path of
    Right ver ->
      -- It's a version
      pure . Just $ HackageDist name (Just ver)
    Left _ ->
      pure Nothing

-- |Try to reconstruct a 'HackageDist' from an existing pkgsrc
-- package. Return 'Nothing' if it's not from Hackage.
reconstructHackageDist :: Package CLI -> CLI (Maybe HackageDist)
reconstructHackageDist pkg =
  do ms <- SrcDb.masterSites pkg
     case ms of
       (m:_) ->
         do distName <- OP.decodeUtf =<< SrcDb.distName    pkg
            sufx     <- OP.decodeUtf =<< SrcDb.extractSufx pkg
            parseHackageDist Nothing $
              m & uriPathLens %~ \base -> base <> distName <> sufx
       _ ->
         pure Nothing

renderHackageDist :: HackageDist -> URI
renderHackageDist (HackageDist name mVer) =
  URI { uriScheme    = ""
      , uriAuthority = Nothing
      , uriPath      = prettyShow name <> foldMap (("-" <>) . prettyShow) mVer
      , uriQuery     = ""
      , uriFragment  = ""
      }

data PackageStatus =
    -- ^This version is good to use.
    Normal
    -- ^This version has known defects.
  | Deprecated
  deriving (Eq, Show)

$(ATH.deriveJSON
   ATH.defaultOptions { ATH.constructorTagModifier = (toLower <$>) }
   ''PackageStatus)

newtype AvailableVersions = AV { unAV :: Map Version PackageStatus }

-- |The JSON representation of this object is like:
-- > {
-- >     "0.0.1": "normal",
-- >     "0.0.2": "deprecated",
-- >     "0.1.1": "normal"
-- > }
instance FromJSON AvailableVersions where
  parseJSON = withObject "AvailableVersions" $
              \obj ->
                AV . M.fromList <$> mapM go (KM.toList obj)
    where
      go (key, val) =
        do ver <- case eitherParsec . T.unpack . K.toText $ key of
                    Right ver -> pure ver
                    Left  e   -> fail (show e)
           st  <- parseJSON val
           pure (ver, st)

-- |Get the latest non-deprecated version. This function is partial because
-- a package may have no preferred versions in theory.
latestPreferred :: HasCallStack => AvailableVersions -> Version
latestPreferred = fst . fromJust . find p . M.toDescList . unAV
  where
    p :: (Version, PackageStatus) -> Bool
    p (_, Normal    ) = True
    p (_, Deprecated) = False

-- |Fetch the set of available versions for a given package.
fetchAvailableVersions :: PackageName -> CLI AvailableVersions
fetchAvailableVersions name =
  do info $ PP.hsep [ "Fetching the set of available versions for"
                    , prettyAnsi name <> "..."
                    ]
     hackage <- hackageURI
     req     <- parseRequest $ uriToString id (avURI hackage) ""
     getResponseBody <$> httpJSON req
  where
    -- Generate a URL like
    -- https://hackage.haskell.org/package/{packageName}.json
    avURI :: URI -> URI
    avURI hackage =
      hackage & uriPathLens %~ \base ->
      FP.joinPath [ base
                  , prettyShow name <> ".json"
                  ]

-- |Fetch a .cabal file from Hackage for a given package.
fetchCabal :: HackageDist -> ConduitT i (PosixPath, ByteString) CLI ()
fetchCabal (HackageDist name mVer) =
  do hackage <- lift hackageURI
     req     <- parseRequest $ uriToString id (cabalURI hackage) ""
     cabal   <- toStrict <$> (httpSource req getCabal .| sinkLazy)
     file    <- OP.encodeUtf cabalFile
     yield (file, cabal)
  where
    -- We generate
    -- https://hackage.haskell.org/package/{packageId}/revision/0.cabal but
    -- not https://hackage.haskell.org/package/{packageId}.cabal, because
    -- the former is exactly what in downloaded tarballs. The latter is
    -- potentially a revised one.
    cabalURI :: URI -> URI
    cabalURI hackage =
      hackage & uriPathLens %~ \base ->
      FP.joinPath [ base
                  , prettyShow name <> foldMap (("-" <>) . prettyShow) mVer
                  , "revision"
                  , "0.cabal"
                  ]

    cabalFile :: FilePath
    cabalFile =
      mconcat [ prettyShow name
              , foldMap (("-" <>) . prettyShow) mVer
              , ".cabal"
              ]

    getCabal :: MonadThrow m
             => Response (ConduitT i ByteString m ())
             -> ConduitT i ByteString m ()
    getCabal res
      | statusIsSuccessful . getResponseStatus $ res =
          case getResponseHeader hContentType res of
            [cType]
              | isCabal cType ->
                  getResponseBody res
            ts ->
              fatal $ PP.hsep [ "Couldn't fetch a package description from Hackage:"
                              , "Bad media type:"
                              , PP.viaShow ts
                              ]
      | otherwise =
          let sc = getResponseStatus res
          in
            fatal $ PP.hsep [ "Couldn't fetch a package description from Hackage:"
                            , PP.pretty (statusCode sc)
                            , PP.pretty (decodeUtf8Lenient . statusMessage $ sc)
                            ]

    isCabal :: ByteString -> Bool
    isCabal cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> MT.mainType mt == "text"  &&
                   MT.subType  mt == "plain" &&
                   elem (mt MT./. "charset") [Nothing, Just "utf-8"]

-- |Fetch a ChangeLog file from Hackage for a given package. Yields nothing
-- if it doesn't have one.
fetchChangeLog :: HackageDist -> ConduitT i (PosixPath, LBS.ByteString) CLI ()
fetchChangeLog (HackageDist name mVer) =
  do hackage   <- lift hackageURI
     req       <- parseRequest $ uriToString id (changeLogURI hackage) ""
     httpSource req getChangeLog
  where
    changeLogURI :: URI -> URI
    changeLogURI hackage =
      hackage & uriPathLens %~ \base ->
      FP.joinPath [ base
                  , prettyShow name <> foldMap (("-" <>) . prettyShow) mVer
                  , "changelog.txt"
                  ]

    getChangeLog :: MonadThrow m
                 => Response (ConduitT i ByteString m ())
                 -> ConduitT i (PosixPath, LBS.ByteString) m ()
    getChangeLog res
      | statusIsSuccessful . getResponseStatus $ res =
          case getResponseHeader hContentType res of
            [cType]
              | isChangeLog cType ->
                  do changeLog <- getResponseBody res .| sinkLazy
                     yield ([pstr|changelog.txt|], changeLog)
            ts ->
              fatal $ PP.hsep [ "Couldn't fetch ChangeLog from Hackage:"
                              , "Bad media type:"
                              , PP.viaShow ts
                              ]
      | (== notFound404) . getResponseStatus $ res =
          -- This is not an error. Some packages have no ChangeLog.
          pure ()
      | otherwise =
          let sc = getResponseStatus res
          in
            fatal $ PP.hsep [ "Couldn't fetch ChangeLog from Hackage:"
                            , PP.pretty (statusCode sc)
                            , PP.pretty (decodeUtf8Lenient . statusMessage $ sc)
                            ]

    isChangeLog :: ByteString -> Bool
    isChangeLog cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> MT.mainType mt == "text" &&
                   elem (mt MT./. "charset") [Nothing, Just "utf-8"]
