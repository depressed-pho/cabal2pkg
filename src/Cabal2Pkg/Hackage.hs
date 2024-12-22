{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |Hackage recommends API users to use the hackage-security library
-- instead of directly accessing it. However, hackage-security is designed
-- to build a clone of the entire database as a local cache and update it
-- from time to time. That doesn't really suit well to our use case. So we
-- directly use its REST API.
module Cabal2Pkg.Hackage
  ( PackageStatus(..)
  , AvailableVersions(..)
  , latestPreferred

    -- * I/O
  , fetchAvailableVersions
  , fetchCabal
  ) where

import Cabal2Pkg.CmdLine (CLI, info, fatal, hackageURI)
import Cabal2Pkg.Pretty (prettyAnsi)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as ATH
import Data.Aeson.Types (FromJSON(..), withObject)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.Conduit ((.|), ConduitT, yield)
import Data.Conduit.Combinators (sinkLazy)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import GHC.Stack (HasCallStack)
import Lens.Micro ((&), (%~))
import Network.HTTP.Media qualified as MT
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpJSON, httpSource, parseRequest )
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), uriToString)
import Network.URI.Lens (uriPathLens)
import Prettyprinter qualified as PP
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix (PosixPath)
import System.OsPath.Posix qualified as OP

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
                  , "package"
                  , prettyShow name <> ".json"
                  ]

-- |Fetch a .cabal file from the Hackage for a given package.
fetchCabal :: PackageName
           -> Maybe Version
           -> ConduitT i (PosixPath, ByteString) CLI ()
fetchCabal name mVer =
  do hackage <- lift hackageURI
     req     <- parseRequest $ uriToString id (cabalURI hackage)""
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
                  , "package"
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
    getCabal res =
      if statusIsSuccessful . getResponseStatus $ res then
        case getResponseHeader hContentType res of
          [cType]
            | isCabal cType ->
                getResponseBody res
          ts ->
            fatal $ PP.hsep [ "Couldn't fetch a package description from Hackage:"
                            , "Bad media type:"
                            , PP.viaShow ts
                            ]
      else
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
