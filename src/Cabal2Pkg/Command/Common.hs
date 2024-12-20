{-# LANGUAGE OverloadedStrings #-}
module Cabal2Pkg.Command.Common
  ( fetchMeta
  ) where

import Cabal2Pkg.Cabal (readCabal)
import Cabal2Pkg.CmdLine (CLI, debug, info)
import Cabal2Pkg.PackageURI (PackageURI, isFromHackage)
import Cabal2Pkg.Extractor
  ( PackageMeta, fillInMasterSites, omitHackageDefaults, summariseCabal )
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Text.Show.Pretty (ppShow)

-- |Fetch a package metadata from a URI. If the package URI starts with
-- "{hackageURI}/package/" or it has no scheme, then it means this package
-- came from Hackage. In that case we omit MASTER_SITES and HOMEPAGE
-- because mk/haskell.mk takes care of them.
fetchMeta :: HasCallStack => PackageURI -> CLI PackageMeta
fetchMeta uri =
  do info $ "Fetching" <+> PP.dquotes (PP.viaShow uri) <> "..."

     cabal <- readCabal uri
     debug $ "Found a package description:\n" <> PP.pretty (ppShow cabal)

     let transMeta = if isFromHackage uri then
                       omitHackageDefaults
                     else
                       fillInMasterSites uri
     meta <- transMeta <$> summariseCabal cabal

     debug $ "Summarised package metadata:\n" <> PP.pretty (ppShow meta)
     pure meta
