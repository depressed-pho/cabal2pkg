{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Cabal2Pkg.Utils
  ( embedEDERelative
  , renderEDE
  ) where

import Control.Exception.Safe (Exception(..), MonadThrow, throw)
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Maybe (fromJust)
import Data.String (IsString(..))
import Data.Text.Lazy qualified as LT
import GHC.Stack (HasCallStack, callStack, popCallStack, prettyCallStack)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Code, unsafeCodeCoerce)
import System.OsPath qualified as OP
import System.OsPath (OsPath)
import Text.EDE (Template)
import Text.EDE qualified as EDE


-- |Embed an EDE template file relative to the project root. Due to a
-- limitation in the ede library, the function cannot parse the template at
-- compilation time, and may throw exceptions at use site. Callers should
-- make sure not to inline the resulting code so that parsing will happen
-- just once.
--
-- Note: We really want this expression to have type @(forall
-- m. HasCallStack, MonadThrow m) => m Template@ rather than just
-- @Template@, but currently typed expressions cannot have constraints.
embedEDERelative :: FilePath -> Code Q Template
embedEDERelative path
  = [|| either error id $ EDE.eitherParse $$bsCode ||]
  where
    bsCode :: Code Q ByteString
    bsCode = unsafeCodeCoerce $ embedFileRelative path

-- |Like 'EDE.eitherRender' but this one takes anything implementing
-- 'ToJSON' as the value, and throws an exception when it fails.
renderEDE :: (HasCallStack, ToJSON a, MonadThrow m) => Template -> a -> m LT.Text
renderEDE tmpl a
  = either (throw . TemplateError) pure $ EDE.eitherRender tmpl bindings
  where
    bindings = fromJust $ EDE.fromValue $ toJSON a


data TemplateError where
  TemplateError :: HasCallStack => String -> TemplateError
  deriving Exception

instance Show TemplateError where
  show (TemplateError e)
    = e <> "\n" <> prettyCallStack (popCallStack callStack)


-- |An orphan instance of IsString for OsPath. No idea why the upstream
-- doesn't provide this.
instance IsString OsPath where
  fromString :: String -> OsPath
  fromString = unsafeEncodeUtf


-- |A shim to unsafeEncodeUtf introduced in filepath-1.5.2
unsafeEncodeUtf :: HasCallStack => String -> OsPath
#if MIN_VERSION_filepath(1, 5, 2)
unsafeEncodeUtf = OP.unsafeEncodeUtf
#else
unsafeEncodeUtf = either (error . show) id . OP.encodeUtf
#endif
