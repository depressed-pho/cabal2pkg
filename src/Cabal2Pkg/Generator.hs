{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Generator
  ( PackageMeta(..)
  , summariseCabal
  ) where

import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.License qualified as LL
import Distribution.Pretty (prettyShow)
import Distribution.SPDX.License qualified as SL
import Distribution.SPDX.LicenseExpression qualified as SL
import Distribution.SPDX.LicenseId qualified as SL
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.GenericPackageDescription qualified as GPD
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Utils.ShortText (fromShortText)
import GHC.Generics (Generic, Generically(..))


data PackageMeta = PackageMeta
  { distName   :: Text
  , categories :: Text
  , maintainer :: Text
  , comment    :: Text
  , license    :: Text
  }
  deriving (Generic, Show)
  deriving ToJSON via Generically PackageMeta


summariseCabal :: Text -> Maybe Text -> GenericPackageDescription -> PackageMeta
summariseCabal category maintainer gpd =
  PackageMeta
  { distName   = T.pack . prettyShow . PD.package $ pd
  , categories = category
  , maintainer = fromMaybe "pkgsrc-users@NetBSD.org" maintainer
  , comment    = T.pack . fromShortText . PD.synopsis $ pd
  , license    = license
  }
  where
    pd :: PackageDescription
    pd = GPD.packageDescription gpd

    license :: Text
    license = either spdxLicense legacyLicense $ PD.licenseRaw pd

spdxLicense :: SL.License -> Text
spdxLicense SL.NONE           = "# TODO: The package contains no license " <>
                                "information whatsoever. Decide what to do with it."
spdxLicense (SL.License expr) = fmtExpr expr
  where
    -- Inefficient, better using builder, but is that worth it?
    fmtExpr :: SL.LicenseExpression -> Text
    fmtExpr le =
      case le of
        SL.ELicense sle Nothing ->
          fmtSLE sle
        e@(SL.ELicense _ (Just _)) ->
          T.pack $ "# TODO: " <> prettyShow e <> ". Decide what to do with it."
        SL.EAnd e0 e1 ->
          paren (fmtExpr e0) <> " AND " <> paren (fmtExpr e1)
        SL.EOr e0 e1 ->
          paren (fmtExpr e0) <> " OR " <> paren (fmtExpr e1)

    paren :: Text -> Text
    paren t
      | T.any (== ' ') t = "(" <> t <> ")"
      | otherwise        = t

    fmtSLE :: SL.SimpleLicenseExpression -> Text
    fmtSLE sle =
      case sle of
        SL.ELicenseId elid ->
          fmtLID elid
        SL.ELicenseIdPlus elid ->
          -- This means "{elid} or later" but pkgsrc doesn't has a notation
          -- for that. For now we just express that with the lower bound.
          fmtLID elid
        SL.ELicenseRef ref ->
          -- Dunno what to do with this…
          T.pack $ prettyShow ref

    fmtLID :: SL.LicenseId -> Text
    fmtLID lid =
      -- LOL, why the fuck are there this many licenses on earth.
      case lid of
        SL.NullBSD           -> "0-clause-bsd"
        SL.AFL_1_1           -> "afl-1.1"
        SL.AFL_1_2           -> "afl-1.2"
        SL.AFL_2_0           -> "afl-2.0"
        SL.AFL_2_1           -> "afl-2.1"
        SL.AFL_3_0           -> "afl-3.0"
        SL.AGPL_1_0          -> "gnu-agpl-v1"
        SL.AGPL_1_0_only     -> "gnu-agpl-v1"
        SL.AGPL_1_0_or_later -> "gnu-agpl-v1 OR gnu-agpl-v3"
        SL.AGPL_3_0_only     -> "gnu-agpl-v3"
        SL.AGPL_3_0_or_later -> "gnu-agpl-v3"
        SL.Aladdin           -> "aladdin-license"
        SL.Apache_1_0        -> "apache-1.0"
        SL.Apache_1_1        -> "apache-1.1"
        SL.Apache_2_0        -> "apache-2.0"
        SL.APSL_2_0          -> "apple-public-source-license"
        SL.Artistic_1_0      -> "artistic"
        SL.Artistic_2_0      -> "artistic-2.0"
        SL.Beerware          -> "beer-ware"
        SL.BSD_1_Clause      -> "1-clause-bsd"
        SL.BSD_2_Clause      -> "2-clause-bsd"
        SL.BSD_3_Clause      -> "modified-bsd"
        SL.BSD_4_Clause      -> "original-bsd"
        SL.BSL_1_0           -> "boost-license"
        SL.CC_BY_1_0         -> "cc-by-v1.0"
        SL.CC_BY_2_0         -> "cc-by-v2.0"
        SL.CC_BY_2_5         -> "cc-by-v2.5"
        SL.CC_BY_3_0         -> "cc-by-v3.0"
        SL.CC_BY_4_0         -> "cc-by-v4.0"
        SL.CC_BY_NC_1_0      -> "cc-by-nc-v1.0-license"
        SL.CC_BY_NC_2_0      -> "cc-by-nc-v2.0-license"
        SL.CC_BY_NC_2_5      -> "cc-by-nc-v2.5-license"
        SL.CC_BY_NC_3_0      -> "cc-by-nc-v3.0-license"
        SL.CC_BY_NC_4_0      -> "cc-by-nc-v4.0-license"
        SL.CC_BY_NC_ND_1_0   -> "cc-by-nc-nd-v1.0-license"
        SL.CC_BY_NC_ND_2_0   -> "cc-by-nc-nd-v2.0-license"
        SL.CC_BY_NC_ND_2_5   -> "cc-by-nc-nd-v2.5-license"
        SL.CC_BY_NC_ND_3_0   -> "cc-by-nc-nd-v3.0-license"
        SL.CC_BY_NC_ND_4_0   -> "cc-by-nc-nd-v4.0-license"
        SL.CC_BY_NC_SA_1_0   -> "cc-by-nc-sa-v1.0-license"
        SL.CC_BY_NC_SA_2_0   -> "cc-by-nc-sa-v2.0-license"
        SL.CC_BY_NC_SA_2_5   -> "cc-by-nc-sa-v2.5-license"
        SL.CC_BY_NC_SA_3_0   -> "cc-by-nc-sa-v3.0-license"
        SL.CC_BY_NC_SA_4_0   -> "cc-by-nc-sa-v4.0-license"
        SL.CC_BY_ND_1_0      -> "cc-by-nd-v1.0-license"
        SL.CC_BY_ND_2_0      -> "cc-by-nd-v2.0-license"
        SL.CC_BY_ND_2_5      -> "cc-by-nd-v2.5-license"
        SL.CC_BY_ND_3_0      -> "cc-by-nd-v3.0-license"
        SL.CC_BY_ND_4_0      -> "cc-by-nd-v4.0-license"
        SL.CC_BY_SA_1_0      -> "cc-by-sa-v1.0"
        SL.CC_BY_SA_2_0      -> "cc-by-sa-v2.0"
        SL.CC_BY_SA_2_5      -> "cc-by-sa-v2.5"
        SL.CC_BY_SA_3_0      -> "cc-by-sa-v3.0"
        SL.CC_BY_SA_4_0      -> "cc-by-sa-v4.0"
        SL.CC0_1_0           -> "cc0-1.0-universal"
        SL.CDDL_1_0          -> "cddl-1.0"
        SL.CNRI_Python       -> "python-software-foundation"
        SL.CPL_1_0           -> "cpl-1.0"
        SL.EGenix            -> "egenix-public-license"
        SL.EPL_1_0           -> "epl-v1.0"
        SL.EPL_2_0           -> "epl-v2.0"
        SL.ErlPL_1_1         -> "erlang-public-license"
        SL.EUPL_1_0          -> "eupl-v1.0"
        SL.EUPL_1_1          -> "eupl-v1.1"
        SL.EUPL_1_2          -> "eupl-v1.2"
        SL.GFDL_1_1_only     -> "gnu-fdl-v1.1"
        SL.GFDL_1_2_only     -> "gnu-fdl-v1.2"
        SL.GFDL_1_3_only     -> "gnu-fdl-v1.3"
        SL.GPL_1_0_only      -> "gnu-gpl-v1"
        SL.GPL_1_0_or_later  -> "gnu-gpl-v1 OR gnu-gpl-v2 OR gnu-gpl-v3"
        SL.GPL_2_0_only      -> "gnu-gpl-v2"
        SL.GPL_2_0_or_later  -> "gnu-gpl-v2 OR gnu-gpl-v2"
        SL.GPL_3_0_only      -> "gnu-gpl-v3"
        SL.GPL_3_0_or_later  -> "gnu-gpl-v3"
        SL.HPND              -> "hpnd"
        SL.IJG               -> "ijg"
        SL.Info_ZIP          -> "info-zip"
        SL.IPA               -> "ipafont"
        SL.IPL_1_0           -> "ipl-1.0"
        SL.ISC               -> "isc"
        SL.LGPL_2_0_only     -> "gnu-lgpl-v2"
        SL.LGPL_2_0_or_later -> "gnu-lgpl-v2 OR gnu-lgpl-v2.1 OR gnu-lgpl-v3"
        SL.LGPL_2_1_only     -> "gnu-lgpl-v2.1"
        SL.LGPL_2_1_or_later -> "gnu-lgpl-v2.1 OR gnu-lgpl-v3"
        SL.LGPL_3_0_only     -> "gnu-lgpl-v3"
        SL.LGPL_3_0_or_later -> "gnu-lgpl-v3"
        SL.LPPL_1_0          -> "lppl-1.0"
        SL.LPPL_1_1          -> "lppl-1.1"
        SL.LPPL_1_2          -> "lppl-1.2"
        SL.LPPL_1_3a         -> "lppl-1.3a"
        SL.LPPL_1_3c         -> "lppl-1.3c"
        SL.MIT               -> "mit"
        SL.MPL_1_0           -> "mpl-1.0"
        SL.MPL_1_1           -> "mpl-1.1"
        SL.MPL_2_0           -> "mpl-2.0"
        SL.MS_PL             -> "ms-pl"
        SL.NGPL              -> "nethack-license"
        SL.ODbL_1_0          -> "odbl-v1"
        SL.OFL_1_0           -> "ofl-v1.0"
        SL.OFL_1_1           -> "ofl-v1.1"
        SL.OpenSSL           -> "openssl"
        SL.OSL_3_0           -> "osl"
        SL.PHP_3_01          -> "php"
        SL.PostgreSQL        -> "postgresql-license"
        SL.PSF_2_0           -> "python-software-foundation"
        SL.QPL_1_0           -> "qpl-v1.0"
        SL.Ruby              -> "ruby-license"
        SL.SGI_B_1_0         -> "sgi-free-software-b-v1.0"
        SL.SGI_B_1_1         -> "sgi-free-software-b-v1.1"
        SL.SGI_B_2_0         -> "sgi-free-software-b-v2.0"
        SL.SISSL             -> "sissl-1.1"
        SL.SISSL_1_2         -> "sissl-1.2"
        SL.Sleepycat         -> "sleepycat-license"
        SL.Unicode_TOU       -> "unicode"
        SL.Unlicense         -> "unlicense"
        SL.Vim               -> "vim-license"
        SL.W3C               -> "w3c"
        SL.X11               -> "x11"
        SL.Zlib              -> "zlib"
        SL.ZPL_1_1           -> "zpl-1.1"
        SL.ZPL_2_0           -> "zpl-2.0"
        SL.ZPL_2_1           -> "zpl-2.1"
        _                    -> T.pack $ "# TODO: Unknown license: " <> prettyShow lid <>
                                         ". Decide what to do with it."

legacyLicense :: LL.License -> Text
legacyLicense l =
  case l of
    LL.GPL  (Just ver)   -> T.pack $ "gnu-gpl-v" <> prettyShow ver
    LL.AGPL (Just ver)   -> T.pack $ "gnu-agpl-v" <> prettyShow ver
    LL.LGPL (Just ver)   -> T.pack $ "gnu-lgpl-v" <> prettyShow ver
    LL.BSD2              -> "2-clause-bsd"
    LL.BSD3              -> "modified-bsd"
    LL.BSD4              -> "original-bsd"
    LL.MIT               -> "mit"
    LL.ISC               -> "isc"
    LL.MPL ver           -> T.pack $ "mpl-" <> prettyShow ver
    LL.Apache (Just ver) -> T.pack $ "apache-" <> prettyShow ver
    LL.PublicDomain      -> "public-domain"
    _                    -> T.pack $ "# TODO: Unknown license: " <> prettyShow l <>
                                     ". Decide what to do with it."
