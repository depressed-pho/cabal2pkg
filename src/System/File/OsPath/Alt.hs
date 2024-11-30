-- |An alternative API to @System.File.OsPath@ from the @file-io@
-- package. All of these operations are performed atomically unless
-- specified otherwise.
module System.File.OsPath.Alt
  ( touchFile
  , writeFile
  , writeFreshFile
  , withFile
  ) where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as Lazy
import Prelude hiding (writeFile)
import System.IO (Handle, hClose)
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString(getOsString))
import System.OsString.Posix (PosixString)
import System.Posix.IO.PosixString
  ( OpenFileFlags(..), OpenMode(..), defaultFileFlags, fdToHandle, openFd )


-- |Create an empty file if it doesn't already exist.
touchFile :: (MonadIO m, MonadMask m) => OsPath -> m ()
touchFile fp = withFile fp WriteOnly flags (const . pure $ ())
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags
            { creat = Just 0o666
            }

-- |Write a lazy 'ByteString' to a file. Existing files will be
-- overwritten.
writeFile :: (MonadIO m, MonadMask m) => OsPath -> Lazy.ByteString -> m ()
writeFile fp bs = withFile fp WriteOnly flags (liftIO . flip Lazy.hPut bs)
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags
            { trunc = True
            , creat = Just 0o666
            }

-- |Write a lazy 'ByteString' to a file, but only when the file doesn't
-- already exist. If it exists the action raises an 'IOError' with
-- 'System.IO.Error.isAlreadyExistsError' returning 'True'.
writeFreshFile :: (MonadIO m, MonadMask m) => OsPath -> Lazy.ByteString -> m ()
writeFreshFile fp bs = withFile fp WriteOnly flags (liftIO . flip Lazy.hPut bs)
  where
    flags :: OpenFileFlags
    flags = defaultFileFlags
            { exclusive = True
            , trunc     = True
            , creat     = Just 0o666
            }

-- |Run an action on a file. The 'Handle' is automatically closed after the
-- action.
withFile :: (MonadIO m, MonadMask m)
              => OsPath
              -> OpenMode
              -> OpenFileFlags
              -> (Handle -> m a)
              -> m a
withFile fp mode flags = bracket open close
  where
    open :: (MonadIO m, MonadMask m) => m Handle
    open = liftIO $ fdToHandle =<< openFd fp' mode flags

    close :: MonadIO m => Handle -> m ()
    close = liftIO . hClose

    fp' :: PosixString
    fp' = getOsString fp
