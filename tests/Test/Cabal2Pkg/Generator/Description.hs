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
  testCase "Examples" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ ">>> fib 10\n"
              , "55\n"
              , "\n"
              , ">>> putStrLn \"foo\\n\\nbar\"\n"
              , "foo\n"
              , "<BLANKLINE>\n"
              , "bar\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ ">>> fib 10\n"
              , "55\n"
              , "\n"
              , ">>> putStrLn \"foo\\n\\nbar\"\n"
              , "foo\n"
              , "\n"
              , "bar\n"
              ]

test =
  testCase "Tables are currently not rendered but they can still be parsed" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "Table:\n"
              , "\n"
              , "+-----+-----+\n"
              , "| abc | def |\n"
              , "+=====+=====+\n"
              , "| foo | bar |\n"
              , "+-----+-----+\n"
              , "| baz | qux |\n"
              , "+-----+-----+\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "Table:\n"
              ]

test =
  testCase "@since is not rendered" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "Foo\n"
              , "\n"
              , "@since 1.2.3\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "Foo\n"
              ]

test =
  testCase "Basic unordered list" $
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
  testCase "Ordered lists" $
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
  testCase "Definition lists" $
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

test =
  testCase "Nested lists" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* Lorem ipsum dolor sit amet,\n"
              , "\n"
              , "    * Foo\n"
              , "    * Bar\n"
              , "\n"
              , "    1. Baz\n"
              , "    2. Qux\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* Lorem ipsum dolor sit amet,\n"
              , "\n"
              , "  * Foo\n"
              , "  * Bar\n"
              , "\n"
              , "  1. Baz\n"
              , "  2. Qux\n"
              ]

test =
  testCase "Code block (verbatim)" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "> @foo@\n"
              , ">   /bar/ baz\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "@foo@\n"
              , "  /bar/ baz\n"
              ]

test =
  testCase "Code block (parsed)" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "@\n"
              , "@foo@\n"
              , "  /bar/ baz\n"
              , "@\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "foo\n"
              , "  bar baz\n"
              ]

test =
  testCase "Properties" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "prop> a + b = b + a"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "prop> a + b = b + a\n"
              ]

test =
  testCase "Heading" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "= Heading level 1 with some /emphasis/"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "# Heading level 1 with some emphasis\n"
              ]

test =
  testCase "Monospaced" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "@foo bar@"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "foo bar\n"
              ]

test =
  testCase "Emphasis" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "/foo bar/"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "foo bar\n"
              ]

test =
  testCase "Bold" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "__foo bar__"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "foo bar\n"
              ]

test =
  testCase "Anchor is invisible" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "foo#anchor#bar"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "foobar\n"
              ]

test =
  testCase "Identifier" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* An obvious 'identifier'\n"
              , "* A mischievous 'identifier''\n"
              , "* An operator '(<$>)'\n"
              , "* A 'Qualified.identifier'\n"
              , "* This isn't an identifier. No it's not.\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* An obvious identifier\n"
              , "* A mischievous identifier'\n"
              , "* An operator (<$>)\n"
              , "* A Qualified.identifier\n"
              , "* This isn't an identifier. No it's not.\n"
              ]

test =
  testCase "Hyperlinks" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* <http://explicit-link.example.org/>\n"
              , "* <http://explicit-link.example.org/ label>\n"
              , "* http://implicit-link.example.org/\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* http://explicit-link.example.org/\n"
              , "* label (http://explicit-link.example.org/)\n"
              , "* http://implicit-link.example.org/\n"
              ]

test =
  testCase "Module links" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* \"Foo\"\n"
              , "* \"Foo.Bar\"\n"
              , "* [markdown style](\"Foo.Bar.Baz\")\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "* Foo\n"
              , "* Foo.Bar\n"
              , "* markdown style\n"
              ]

test =
  testCase "Pictures" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "* inline <<picture.png>>\n"
              , "* inline <<picture.png label>>\n"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      -- This isn't ideal as there shouldn't be a trailing whitespace. But
      -- we can't make an effort just for rare cases where package
      -- descriptions have images.
      mconcat [ "* inline \n"
              , "* inline \n"
              ]

test =
  testCase "Block Math" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "\\[f(a) = \\frac{1}{a}\\]"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "f(a) = \\frac{1}{a}\n"
              ]

test =
  testCase "Inline Math" $
  genDESCR 76 (dummyMeta { description = source }) @?= rendered
  where
    source :: (IsString a, Monoid a) => a
    source =
      mconcat [ "\\(f(a) = \\frac{1}{a}\\)"
              ]
    rendered :: (IsString a, Monoid a) => a
    rendered =
      mconcat [ "f(a) = \\frac{1}{a}\n"
              ]
