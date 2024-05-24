{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency
  ( Dependency(..)
  , extractDependency
  ) where

import Cabal2Pkg.CmdLine (CLI, installedPkgs)
import Distribution.Types.Dependency qualified as C
import Data.Aeson ((.=), ToJSON(..), Value, object)
import Data.Text (Text)


-- |Dependency on a pkgsrc package.
data Dependency
  = KnownDependency
    { -- |The PKGPATH, such as @"math/hs-semigroupoids"@.
      pkgPath :: !Text
      -- |Whether the package needs to be listed in
      -- @HASKELL_UNRESTRICT_DEPENDENCIES@.
    , needsUnrestricting :: !Bool
    }
  | UnknownDependency
    { -- |The name of Cabal package, such as @"semigroupoids"@. This
      -- constructor is used when 'Cabal2Pkg.Extractor.summariseCabal'
      -- cannot find the corresponding package in pkgsrc or bundled
      -- libraries in GHC.
      name :: !Text
    }
  deriving Show

instance ToJSON Dependency where
  toJSON :: Dependency -> Value
  toJSON dep
    = case dep of
        KnownDependency {..} ->
          object [ "kind"               .= ("known" :: Text)
                 , "pkgPath"            .= pkgPath
                 , "needsUnrestricting" .= needsUnrestricting
                 ]
        UnknownDependency {..} ->
          object [ "kind" .= ("unknown" :: Text)
                 , "name" .= name
                 ]

-- |Return 'False' if the dependency is bundled with the compiler.
extractDependency :: C.Dependency -> CLI (Maybe Dependency)
extractDependency dep
  = do ipi <- installedPkgs
       pure Nothing
