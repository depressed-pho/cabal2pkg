{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}
module Test.Cabal2Pkg.Generator.Description
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Cabal2Pkg.Extractor (PackageMeta(..))
import Cabal2Pkg.Generator.Description (genDESCR)
import Cabal2Pkg.Site (PackageURI(File))
import Data.String (IsString)
import Distribution.Types.Version (mkVersion)
import Test.Tasty.HUnit ((@?=), testCase)


dummyMeta :: PackageMeta
dummyMeta = PackageMeta
            { distBase    = "foo"
            , distVersion = mkVersion [0, 1]
            , pkgBase     = "hs-foo"
            , pkgPath     = "devel/hs-foo"
            , categories  = ["devel"]
            , origin      = File "foo-0.1.tar.gz"
            , maintainer  = Nothing
            , owner       = Nothing
            , homepage    = "http://example.org/"
            , comment     = "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
            , description = ""
            , license     = "2-clause-bsd"
            , changeLog   = Nothing
            , flags       = mempty
            , components  = mempty
            }

test =
  testCase "Lines are filled correctly" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n"
              , "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n"
              , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris\n"
              , "@nisi@ @ut@ @aliquip@ @ex@ @ea@ @commodo@ @consequat@.\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod\n"
              , "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,\n"
              , "quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo\n"
              , "consequat.\n"
              ]

test =
  testCase "Newlines in an unordered list item are converted to spaces" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* Lorem ipsum dolor sit amet, consectetur\n"
              , "adipiscing elit,\n"
              , "* sed do eiusmod tempor incididunt ut labore et\n"
              , "dolore magna aliqua. Ut enim ad minim veniam, quis\n"
              , "nostrud exercitation ullamco laboris nisi ut aliquip\n"
              , "ex ea commodo consequat."
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n"
              , "* sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim\n"
              , "  ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip\n"
              , "  ex ea commodo consequat.\n"
              ]

test =
  testCase "Ditto for ordered lists" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "1. Lorem ipsum dolor sit amet, consectetur\n"
              , "adipiscing elit,\n"
              , "2. sed do eiusmod tempor incididunt ut labore et\n"
              , "dolore magna aliqua. Ut enim ad minim veniam, quis\n"
              , "nostrud exercitation ullamco laboris nisi ut aliquip\n"
              , "ex ea commodo consequat."
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "1. Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n"
              , "2. sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut\n"
              , "   enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut\n"
              , "   aliquip ex ea commodo consequat.\n"
              ]

test =
  testCase "Ditto for definition lists" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "[Lorem]: ipsum dolor sit amet, consectetur\n"
              , "adipiscing elit,\n"
              , "[sed]: do eiusmod tempor incididunt ut labore et\n"
              , "dolore magna aliqua. Ut enim ad minim veniam, quis\n"
              , "nostrud exercitation ullamco laboris nisi ut aliquip\n"
              , "ex ea commodo consequat."
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* Lorem:\n"
              , "    ipsum dolor sit amet, consectetur adipiscing elit,\n"
              , "* sed:\n"
              , "    do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim\n"
              , "    ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut\n"
              , "    aliquip ex ea commodo consequat.\n"
              ]
