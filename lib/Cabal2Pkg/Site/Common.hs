module Cabal2Pkg.Site.Common
  ( isIn
  ) where

import Data.List qualified as L
import Network.URI (URI(..), pathSegments)


isIn :: URI -> URI -> Bool
isIn a b =
  uriScheme    a == uriScheme    b &&
  uriAuthority a == uriAuthority b &&
  pathSegments b `L.isPrefixOf` pathSegments a
