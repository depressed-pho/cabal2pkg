module Cabal2Pkg.Pretty
  ( PrettyAnsi(..)
  ) where

import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP
import Network.URI (URI)
import System.OsPath (OsString)
import System.OsPath qualified as OP
import System.OsPath.Posix (PosixString)
import System.OsPath.Posix qualified as OPP

class PrettyAnsi a where
  prettyAnsi :: a -> Doc AnsiStyle

instance PrettyAnsi OsString where
  prettyAnsi = PP.dquotes . PP.annotate (PP.colorDull PP.Cyan) . ppr'
    where
      ppr' :: OsString -> Doc ann
      ppr' = either (error . show) PP.pretty . OP.decodeUtf

instance PrettyAnsi PosixString where
  prettyAnsi = PP.dquotes . PP.annotate (PP.colorDull PP.Cyan) . ppr'
    where
      ppr' :: PosixString -> Doc ann
      ppr' = either (error . show) PP.pretty . OPP.decodeUtf

instance PrettyAnsi PackageName where
  prettyAnsi = PP.annotate (PP.colorDull PP.Green) . PP.pretty . prettyShow

instance PrettyAnsi Version where
  prettyAnsi = PP.annotate (PP.colorDull PP.Green) . PP.pretty . prettyShow

instance PrettyAnsi URI where
  prettyAnsi = PP.dquotes . PP.annotate (PP.colorDull PP.Cyan) . PP.viaShow
