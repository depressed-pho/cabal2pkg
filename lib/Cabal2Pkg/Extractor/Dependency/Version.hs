module Cabal2Pkg.Extractor.Dependency.Version
  ( cmpRange
  ) where

import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Types.VersionRange qualified as C
import Distribution.Types.VersionInterval qualified as C


-- |Compare a 'Version' against a 'VersionRange. Return @'Just' 'EQ'@ if
-- it's within the range. @'Just' 'LT'@ if it's too old. @'Just' 'GT'@ if
-- it's too new. 'Nothing' if the range is empty.
cmpRange :: Version -> VersionRange -> Maybe Ordering
cmpRange ver range
  | C.withinRange ver range       = Just EQ
  | C.withinRange ver lowRelaxed  = Just LT
  | C.withinRange ver highRelaxed = Just GT
  | otherwise                     = Nothing
  where
    lowRelaxed :: VersionRange
    lowRelaxed = C.fromVersionIntervals
                 . C.relaxHeadInterval
                 . C.toVersionIntervals
                 $ range

    highRelaxed :: VersionRange
    highRelaxed = C.fromVersionIntervals
                 . C.relaxLastInterval
                 . C.toVersionIntervals
                 $ range
