-- |An alternative API to @System.Directory.OsPath@ from the @directory@
-- package, which operates on 'PosixPath' instead of
-- 'System.OsPath.OsPath'.
module System.Directory.PosixPath
  ( createDirectoryIfMissing
  , removePathForcibly
  , canonicalizePath
  , doesFileExist
  , doesDirectoryExist
  , findExecutable
  , listDirectory
  , renameFile
  ) where

import Control.Monad.IO.Unlift (MonadIO, liftIO)
import System.Directory.OsPath qualified as D
import System.OsString.Internal.Types (OsString(..))
import System.OsPath.Posix (PosixPath)


createDirectoryIfMissing :: MonadIO m => Bool -> PosixPath -> m ()
createDirectoryIfMissing =
  (liftIO .) . (. OsString) . D.createDirectoryIfMissing

removePathForcibly :: MonadIO m => PosixPath -> m ()
removePathForcibly =
  liftIO . D.removePathForcibly . OsString

canonicalizePath :: MonadIO m => PosixPath -> m PosixPath
canonicalizePath =
  (getOsString <$>) . liftIO . D.canonicalizePath . OsString

doesFileExist :: MonadIO m => PosixPath -> m Bool
doesFileExist =
  liftIO . D.doesFileExist . OsString

doesDirectoryExist :: MonadIO m => PosixPath -> m Bool
doesDirectoryExist =
  liftIO . D.doesDirectoryExist . OsString

findExecutable :: MonadIO m => PosixPath -> m (Maybe PosixPath)
findExecutable =
  ((getOsString <$>) <$>) . liftIO . D.findExecutable . OsString

listDirectory :: MonadIO m => PosixPath -> m [PosixPath]
listDirectory =
  ((getOsString <$>) <$>) . liftIO . D.listDirectory . OsString

renameFile :: MonadIO m => PosixPath -> PosixPath -> m ()
renameFile from to =
  liftIO $ D.renameFile (OsString from) (OsString to)
