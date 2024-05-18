{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module Cabal2Pkg.Utils
  ( embedMustacheRelative
  ) where

import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (makeRelativeToProject)
import Data.String (IsString(..))
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Code, bindCode, liftData, unsafeCodeCoerce)
import System.FilePath qualified as FP
#if !MIN_VERSION_filepath(1, 5, 2)
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.OsPath qualified as OP
import System.OsPath (OsPath)
import Text.Microstache ( PName(..), Template, compileMustacheText )


-- |Embed a Mustache template file relative to project root. This function
-- parses the template at compilation time, and guarantees that no parsing
-- errors can happen at runtime.
embedMustacheRelative :: FilePath -> Code Q Template
embedMustacheRelative path =
  do relPath <- makeRelativeToProject path
     text    <- either throwIO pure =<< TL.decodeUtf8' <$>
                liftIO (BL.readFile relPath)
     let pName = PName . T.pack $ FP.takeBaseName path
     -- Can't use compileMustacheFile because it is locale-dependent. I
     -- think it's an API bug.
     either throwIO pure (compileMustacheText pName text)
  `bindCode` (unsafeCodeCoerce . liftData)


-- |An orphan instance of IsString for OsPath. No idea why the upstream has
-- this.
instance IsString OsPath where
  fromString :: String -> OsPath
  fromString = unsafeEncodeUtf


-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: String -> OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = unsafePerformIO . OP.encodeUtf
#endif
