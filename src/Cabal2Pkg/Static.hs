module Cabal2Pkg.Static
  ( makeQ
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH.Syntax (Q, Code(..), Lift(..))
import System.Directory.OsPath (findExecutable)
import System.Environment (lookupEnv)
import System.OsPath (OsPath)
import System.OsPath qualified as OP


-- |The path to either @bmake@ or @make@, determined at the compile
-- time. If an environment variable @MAKE@ is defined it will always be
-- used.
makeQ :: Code Q OsPath
makeQ = Code $ getMake >>= examineCode . liftTyped
  where
    getMake :: Q OsPath
    getMake
      = do m0 <- liftIO $ lookupEnv "MAKE"
           case m0 of
             Just p0 ->
               do p0' <- liftIO $ findExecutable =<< OP.encodeUtf p0
                  maybe
                    (fail $ "The environment variable MAKE is set to `"
                            <> p0 <> "' but it is not found")
                    pure p0'
             Nothing ->
               do m1 <- liftIO $ findExecutable =<< OP.encodeUtf "bmake"
                  m2 <- liftIO $ findExecutable =<< OP.encodeUtf "make"
                  maybe (fail "Neither `bmake' nor `make' could be found in PATH") pure
                    $ m1 <|> m2
