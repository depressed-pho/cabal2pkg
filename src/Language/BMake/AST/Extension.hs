{-# LANGUAGE TypeFamilies #-}
-- |Extension points of the AST in Trees that Grow style:
-- https://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf
module Language.BMake.AST.Extension
  ( XComment
  , XBlank
  , XAssignment
  , XValue
  , XDependency
  , XShellCmd
  , XInclude
  , XMessage
  , XExport
  , XExportAll
  , XUnexportEnv
  , XUndef
  , XIf
  , XIfdef
  , XIfmake
  , XNot
  , XOr
  , XAnd
  , XExpr
  , XECompare
  ) where

type family XComment     x
type family XBlank       x
type family XAssignment  x
type family XValue       x
type family XDependency  x
type family XShellCmd    x
type family XInclude     x
type family XMessage     x
type family XExport      x
type family XExportAll   x
type family XUnexportEnv x
type family XUndef       x
type family XIf          x
type family XIfdef       x
type family XIfmake      x
type family XNot         x
type family XOr          x
type family XAnd         x
type family XExpr        x
type family XECompare    x
