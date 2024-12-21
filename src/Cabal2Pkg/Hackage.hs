{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |Hackage recommends API users to use the hackage-security library
-- instead of directly accessing it. However, hackage-security is designed
-- to build a clone of the entire database as a local cache and update it
-- from time to time. That doesn't really suit well to our use case. So we
-- directly use its REST API.
module Cabal2Pkg.Hackage
  ( AvailableVersions(..)
  , fetchHackageVersions
  , fetchHackageCabal
  ) where

import Cabal2Pkg.CmdLine (CLI, fatal, hackageURI)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as ATH
import Data.Aeson.Types (FromJSON(..), withObject)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.Conduit ((.|), ConduitT, yield)
import Data.Conduit.Combinators (sinkLazy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Lens.Micro ((&), (%~))
import Network.HTTP.Media qualified as MT
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpJSON, httpSource, parseRequest )
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), uriToString)
import Network.URI.Lens (uriPathLens)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import System.FilePath.Posix qualified as FP
import System.OsPath.Posix (PosixPath)
import System.OsPath.Posix qualified as OPP

data PackageStatus =
    -- ^This version is good to use.
    Normal
    -- ^This version has a known defect.
  | Deprecated
  deriving (Eq, Show)

$(ATH.deriveJSON
   ATH.defaultOptions { ATH.constructorTagModifier = map toLower }
   ''PackageStatus)

newtype AvailableVersions = AV (Map Version PackageStatus)

-- |The JSON representation of this object is like:
-- > {
-- >     "0.0.1": "normal",
-- >     "0.0.2": "deprecated",
-- >     "0.1.1": "normal"
-- > }
instance FromJSON AvailableVersions where
  parseJSON = withObject "AvailableVersions" $
              \obj ->
                AV . M.fromList <$> mapM go (M.toList $ KM.toMapText obj)
    where
      go (key, val) =
        do ver <- case eitherParsec $ T.unpack key of
                    Right ver -> pure ver
                    Left  e   -> fail (show e)
           st  <- parseJSON val
           pure (ver, st)

-- |Fetch a set of available versions for a given package.
fetchHackageVersions :: PackageName -> CLI AvailableVersions
fetchHackageVersions name =
  do hackage <- hackageURI
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
fetchHackageCabal :: PackageName
                  -> Maybe Version
                  -> ConduitT i (PosixPath, ByteString) CLI ()
fetchHackageCabal name mVer =
  do hackage <- lift hackageURI
     req     <- parseRequest $ uriToString id (cabalURI hackage)""
     cabal   <- toStrict <$> (httpSource req getCabal .| sinkLazy)
     file    <- OPP.encodeUtf cabalFile
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
                  , prettyShow name <> maybe mempty (("-" <>) . prettyShow) mVer
                  , "revision"
                  , "0.cabal"
                  ]

    cabalFile :: FilePath
    cabalFile =
      mconcat [ prettyShow name
              , maybe mempty (("-" <>) . prettyShow) mVer
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
            fatal ( "Couldn't fetch a package description from Hackage:" <+>
                    "Bad media type:" <+>
                    PP.viaShow ts )
      else
        let sc = getResponseStatus res
        in
          fatal ( "Couldn't fetch a package description from Hackage:" <+>
                  PP.pretty (statusCode sc) <+>
                  PP.pretty (decodeUtf8Lenient . statusMessage $ sc) )

    isCabal :: ByteString -> Bool
    isCabal cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> MT.mainType mt == "text"  &&
                   MT.subType  mt == "plain" &&
                   elem (mt MT./. "charset") [Nothing, Just "utf-8"]
