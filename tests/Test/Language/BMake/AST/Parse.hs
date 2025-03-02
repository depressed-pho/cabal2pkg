{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Language.BMake.AST.Parse
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy qualified as TL
import Language.BMake.AST (parseMakefile, exactPrintMakefile)
import Language.BMake.AST.ExactPrint (ExactPrint, EndOfLine(..))
import Language.BMake.AST.Types
import Test.Tasty.HUnit ((@?=), testCase)

ex00 :: TL.Text
ex00 = $(makeRelativeToProject "tests/data/example-00.mk" >>= embedStringFile)

ex00' :: Makefile ExactPrint
ex00' = Makefile
        [ BBlank $ Blank ("", EOL) (Just " $NetBSD$")
        , BBlank $ Blank ("", EOL) Nothing
        , BAssignment $ Assignment ("", "\t", EOL) (Value "" "FOO") Set [Value "" "foo"] Nothing
        , BAssignment $ Assignment ("", "\t", EOL) (Value "" "BAR") Append [Value " " "bar", Value "" "baz"] Nothing
        , BBlank $ Blank ("", EOL) Nothing
        , BDirective . DInclude $ Include ("", "", " ", "", EOL) Normal User "../../foo.mk" Nothing
        ]

test =
  testCase "parse example-00.mk" $
  parseMakefile ex00 @?= Right ex00'

test =
  testCase "roundtrip example-00.mk" $
  exactPrintMakefile ex00' @?= ex00

ex01 :: TL.Text
ex01 = $(makeRelativeToProject "tests/data/example-01.mk" >>= embedStringFile)

ex01' :: Makefile ExactPrint
ex01' = Makefile
        [ BDirective . DConditional $
          Conditional () ( NE.fromList
                           [ CondBranch
                             (Ifdef ("", "") True (Expr (" ", "") (Value "" "FOO")) Nothing)
                             (Makefile [ BDirective . DInclude $
                                         Include ("", "  ", " ", "", EOL) Normal User "foo.mk" Nothing
                                       ])
                           ]
                         ) Nothing (EndIf ("", "", "", EOL) Nothing)
        ]

test =
  testCase "parse example-01.mk" $
  parseMakefile ex01 @?= Right ex01'

test =
  testCase "roundtrip example-01.mk" $
  exactPrintMakefile ex01' @?= ex01
