{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal2Pkg.RawMeta
  ( RawMeta(..)
  , readRawMeta
  ) where

import Cabal2Pkg.Pretty (prettyAnsi)
import Cabal2Pkg.Site (PackageURI(..), renderPackageURI)
import Cabal2Pkg.Site.GitHub (renderGitHubDist)
import Cabal2Pkg.Site.GitLab (renderGitLabDist)
import Cabal2Pkg.Site.Hackage qualified as Hackage
import Cabal2Pkg.CmdLine (CLI, fatal, warn)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Conduit ((.|), ConduitT, runConduit, yield, awaitForever)
import Data.Conduit.Combinators (sinkLazy, sourceFile)
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Tar (FileInfo(filePath), untar)
import Data.Conduit.Zlib (ungzip)
import Data.Monoid (Alt(..))
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Distribution.PackageDescription.Parsec qualified as DPP
import Distribution.Parsec.Warning (showPWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import GHC.Generics (Generic, Generically(..))
import Network.HTTP.Simple
  ( Response, getResponseBody, getResponseHeader, getResponseStatus
  , httpSource, parseRequest )
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as MT
import Network.HTTP.Types
  ( hContentType, statusCode, statusMessage, statusIsSuccessful )
import Network.URI (URI(..), uriToString)
import Prelude hiding (pi)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import System.OsPath.Posix (PosixPath, pstr)
import System.OsPath.Posix qualified as OP
import System.OsString.Posix qualified as OS


data RawMeta =
  RawMeta
  { rmPackageURI :: !PackageURI
  , rmGPD        :: !GenericPackageDescription
  , rmChangeLog  :: !(Maybe TL.Text)
  }

-- It would be nice if we could easily derive this type with some
-- TemplateHaskell trick, but using generic-sop (or records-sop) just for
-- this would be ridiculously overkill.
data PartialRawMeta =
  PartialRawMeta
  { prmGPD       :: !(Alt Maybe GenericPackageDescription)
  , prmChangeLog :: !(Alt Maybe TL.Text)
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via Generically PartialRawMeta

readRawMeta :: PackageURI -> CLI RawMeta
readRawMeta uri =
  do prm <- runConduit $ fetchCabal uri .| C.fold
     case prmGPD prm of
       Alt (Just gpd) ->
         pure $ RawMeta { rmPackageURI = uri
                        , rmGPD        = gpd
                        , rmChangeLog  = getAlt (prmChangeLog prm)
                        }
       _ ->
         do uri' <- renderPackageURI uri
            fatal $ "Can't find any .cabal files in" <+> PP.viaShow uri'

fetchCabal :: PackageURI
           -> ConduitT i PartialRawMeta CLI ()
fetchCabal (HTTP    uri ) = fetchHTTP uri
fetchCabal (File    path) = fetchLocal path
fetchCabal (GitHub  dist) = fetchHTTP =<< lift (renderGitHubDist dist)
fetchCabal (GitLab  dist) = fetchHTTP =<< lift (renderGitLabDist dist)
fetchCabal (Hackage dist) =
  do Hackage.fetchCabal dist
       .| awaitForever (\(path, cabal) ->
                           do gpd <- lift $ parseCabal path cabal
                              yield $ mempty { prmGPD = Alt $ Just gpd })
     Hackage.fetchChangeLog dist
       .| awaitForever (\(path, cl) ->
                           do cl' <- lift $ parseText path cl
                              yield $ mempty { prmChangeLog = Alt $ Just cl' })

fetchLocal :: FilePath -> ConduitT i PartialRawMeta CLI ()
fetchLocal path =
  sourceFile path .| extractMetadataFromTarball

extractMetadataFromTarball :: ConduitT ByteString PartialRawMeta CLI ()
extractMetadataFromTarball = ungzip .| untar findMeta
  where
    findMeta :: FileInfo -> ConduitT ByteString PartialRawMeta CLI ()
    findMeta fi =
      do path <- OS.fromBytes . filePath $ fi
         case OP.splitPath path of
           [_root, file]
             | isCabal file ->
                 do cabal <- toStrict <$> sinkLazy
                    gpd   <- lift $ parseCabal path cabal
                    yield $ mempty { prmGPD = Alt $ Just gpd }
             | isChangeLog file ->
                 do lbs       <- sinkLazy
                    changeLog <- lift $ parseText path lbs
                    yield $ mempty { prmChangeLog = Alt $ Just changeLog }
           _ ->
             pure ()

    isCabal :: PosixPath -> Bool
    isCabal = ([pstr|.cabal|] `OP.isExtensionOf`)

    -- See https://github.com/haskell/hackage-server/blob/5252a9cb922a3f8c02162881c619cae0b84214d9/src/Distribution/Server/Packages/ChangeLog.hs#L11
    isChangeLog :: PosixPath -> Bool
    isChangeLog file =
      let (base, ext) = bimap CI.mk CI.mk $ OP.splitExtension file
          basenames   = CI.mk <$> [ [pstr|NEWS|]
                                  , [pstr|ChangeLog|]
                                  , [pstr|Change_Log|]
                                  , [pstr|Changes|]
                                  ]
          extensions  = CI.mk <$> [ mempty
                                  , [pstr|.txt|]
                                  , [pstr|.md|]
                                  , [pstr|.markdown|]
                                  ]
      in
        base `elem` basenames &&
        ext  `elem` extensions

parseCabal :: PosixPath -> ByteString -> CLI GenericPackageDescription
parseCabal cabalPath cabal =
  case DPP.runParseResult $ DPP.parseGenericPackageDescription cabal of
    (ws, Right gpd) ->
      do path' <- OP.decodeUtf cabalPath
         mapM_ (warn . PP.pretty . showPWarning path') ws
         pure gpd
    (_, Left e) ->
      fatal $ PP.hsep [ "Cannot parse"
                      , prettyAnsi cabalPath <> PP.colon
                      , PP.viaShow e
                      ]

parseText :: PosixPath -> LBS.ByteString -> CLI TL.Text
parseText textPath lbs =
  case TL.decodeUtf8' lbs of
    Right text -> pure text
    Left  e    ->
      fatal $ PP.hsep [ "Cannot decode"
                      , prettyAnsi textPath <> PP.colon
                      , PP.viaShow e
                      ]

fetchHTTP :: URI -> ConduitT i PartialRawMeta CLI ()
fetchHTTP uri =
  do req <- parseRequest $ uriToString id uri ""
     httpSource req getTarball .| extractMetadataFromTarball
  where
    getTarball :: MonadThrow m
               => Response (ConduitT i ByteString m ())
               -> ConduitT i ByteString m ()
    getTarball res =
      if statusIsSuccessful . getResponseStatus $ res then
        case getResponseHeader hContentType res of
          [cType]
            | isTarball cType ->
                getResponseBody res
          ts ->
            fatal ( "Couldn't fetch a package tarball from" <+>
                    prettyAnsi uri <>
                    PP.colon <+>
                    "Bad media type:" <+>
                    PP.viaShow ts )
      else
        let sc = getResponseStatus res
        in
          fatal ( "Couldn't fetch a package tarball from" <+>
                  prettyAnsi uri <>
                  PP.colon <+>
                  PP.pretty (statusCode sc) <+>
                  PP.pretty (decodeUtf8Lenient . statusMessage $ sc) )

    isTarball :: ByteString -> Bool
    isTarball cType =
      case MT.parseAccept cType of
        Nothing -> False
        Just mt -> any (MT.matches mt) tarballTypes

    tarballTypes :: [MediaType]
    tarballTypes =
      [ "application/gzip"       -- RFC 6713
      , "application/tar+gzip"   -- Non-standard
      , "application/x-gzip"     -- Non-standard
      , "application/x-tar+gzip" -- Non-standard
      ]
