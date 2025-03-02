module Cabal2Pkg.Static
  ( makeQ
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Unlift (liftIO)
import Language.Haskell.TH.Syntax (Q, Code(..), Lift(..))
import System.Directory.PosixPath (findExecutable)
import System.Environment (lookupEnv)
import System.OsPath.Posix (PosixPath)
import System.OsPath.Posix qualified as OP


-- |The path to either @bmake@ or @make@, determined at the compile
-- time. If an environment variable @MAKE@ is defined it will always be
-- used.
makeQ :: Code Q PosixPath
makeQ = Code $ getMake >>= examineCode . liftTyped
  where
    getMake :: Q PosixPath
    getMake
      = do m0 <- liftIO $ lookupEnv "MAKE"
           case m0 of
             Just p0 ->
               do p0' <- findExecutable =<< OP.encodeUtf p0
                  case p0' of
                    Nothing ->
                      fail $ mconcat [ "The environment variable MAKE is set to `"
                                     , p0
                                     , "' but it is not found"
                                     ]
                    Just _ ->
                      OP.encodeUtf p0 -- p0, not p0'. This isn't a mistake.
             Nothing ->
               do m1 <- findExecutable =<< OP.encodeUtf "bmake"
                  m2 <- findExecutable =<< OP.encodeUtf "make"
                  maybe (fail "Neither `bmake' nor `make' could be found in PATH") pure
                    $ m1 <|> m2
