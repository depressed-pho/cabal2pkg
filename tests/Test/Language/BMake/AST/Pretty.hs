{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Language.BMake.AST.Pretty
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text.Lazy qualified as TL
import Language.BMake.AST.Plain
import Language.BMake.AST.Types
import Test.Tasty.HUnit ((@?=), testCase)

-- example-00
ex00 :: TL.Text
ex00 = $(makeRelativeToProject "tests/data/example-00.mk" >>= embedStringFile)

ex00' :: Makefile PlainAST
ex00' = Makefile
        [ blank # "$NetBSD$"
        , blank
        , "FOO" .=  ["foo"]
        , "BAR" .+= ["bar", "baz"]
        , blank
        , include "../../foo.mk"
        ]

test =
  testCase "generate example-00.mk" $
  prettyPrintMakefile ex00' @?= ex00
