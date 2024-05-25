{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cabal2Pkg.Extractor.Dependency
  ( Dependency(..)
  , extractDependency
  ) where

import Cabal2Pkg.CmdLine (CLI, installedPkgs)
import Distribution.Simple.PackageIndex qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.InstalledPackageInfo qualified as C
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version (Version)
import Data.Aeson ((.=), ToJSON(..), Value, object)
import Data.List (isSuffixOf)
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

-- |Return 'Nothing' if the dependency is bundled with the compiler.
extractDependency :: C.Dependency -> CLI (Maybe Dependency)
extractDependency dep
  = do ipi <- installedPkgs
       if isBuiltin ipi (C.depPkgName dep)
         then pure Nothing
         else do m <- findPkgsrcPkg (C.depPkgName dep)
                 fail (show m)

isBuiltin :: C.InstalledPackageIndex -> C.PackageName -> Bool
isBuiltin ipi name
  = case C.lookupPackageName ipi name of
      ((_, (pkg:_)):_) ->
        -- NOTE: The package database does not have explicit fields
        -- indicating whether the package is bundled with the compiler. For
        -- now we consider packages whose "hs-libraries" field ends with
        -- "-inplace" to be bundled ones, but this is a fragile test.
        case C.hsLibraries pkg of
          (lib:_) -> "-inplace" `isSuffixOf` lib
          _       -> False
      _ ->
        False

findPkgsrcPkg :: C.PackageName -> CLI (Maybe (Text, Version))
findPkgsrcPkg name
  = error "FIXME"
