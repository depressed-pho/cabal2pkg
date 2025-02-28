{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Language.BMake.AST.Parse
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Language.BMake.AST.ExactPrint (EndOfLine(..), parseMakefile)
import Language.BMake.AST.Types
import Test.Tasty.HUnit ((@?=), testCase)

test =
  testCase "parse an example Makefile" $ do
  parseMakefile $(makeRelativeToProject "tests/data/example-00.mk" >>= embedStringFile)
  @?=
  ( Right $ Makefile
    [ BBlank $ Blank ("", EOL) (Just " $NetBSD$")
    , BBlank $ Blank ("", EOL) Nothing
    , BAssignment $ Assignment ("", "\t", EOL) (Value "" "FOO") Set [Value "" "foo"] Nothing
    , BAssignment $ Assignment ("", "\t", EOL) (Value "" "BAR") Append [Value " " "bar", Value "" "baz"] Nothing
    , BBlank $ Blank ("", EOL) Nothing
    , BDirective . DInclude $ Include ("", "", " ", EOL) Normal User "../../foo.mk" Nothing
    ]
  )
