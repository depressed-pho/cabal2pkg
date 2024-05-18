{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cabal2Pkg.Utils
  ( embedMustacheRelative
  , renderMustacheE
  ) where

import Control.Exception.Safe (Exception(..), MonadThrow, throw)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (makeRelativeToProject)
import Data.String (IsString(..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import GHC.Stack (HasCallStack, callStack, popCallStack, prettyCallStack)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Code, bindCode, liftData, unsafeCodeCoerce)
import System.FilePath qualified as FP
#if !MIN_VERSION_filepath(1, 5, 2)
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.OsPath qualified as OP
import System.OsPath (OsPath)
import Text.Microstache
  ( MustacheWarning, PName(..), Template, compileMustacheText, renderMustacheW )


-- |Embed a Mustache template file relative to project root. This function
-- parses the template at compilation time, and guarantees that no parsing
-- errors can happen at runtime.
embedMustacheRelative :: FilePath -> Code Q Template
embedMustacheRelative path =
  do relPath <- makeRelativeToProject path
     text    <- either throw pure =<< LT.decodeUtf8' <$>
                liftIO (BL.readFile relPath)
     let pName = PName . T.pack $ FP.takeBaseName path
     -- Can't use compileMustacheFile because it is locale-dependent. I
     -- think it's an API bug.
     either throw pure (compileMustacheText pName text)
  `bindCode` (unsafeCodeCoerce . liftData)


-- |Like 'renderMustacheW' but this one takes anything implementing
-- 'ToJSON' as the value, and throws an error when it emits any
-- 'MustacheWarning'.
renderMustacheE :: (HasCallStack, ToJSON a, MonadThrow m) => Template -> a -> m LT.Text
renderMustacheE tmpl a
  = case renderMustacheW tmpl (toJSON a) of
      ([], t) -> pure t
      (ws, _) -> throw $ MustacheError ws

data MustacheError where
  MustacheError :: HasCallStack => [MustacheWarning] -> MustacheError
  deriving Exception

instance Show MustacheError where
  show (MustacheError ws)
    = concat [ "Mustache warnings:\n"
             , concat (showW <$> ws)
             , prettyCallStack (popCallStack callStack)
             ]
    where
      showW :: MustacheWarning -> String
      showW w = "- " <> show w <> "\n"


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
