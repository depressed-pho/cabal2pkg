module Control.Concurrent.Deferred
  ( Deferred
  , defer
  , force
  ) where

import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import UnliftIO.MVar (MVar, modifyMVar, newMVar, readMVar)


-- |@'Deferred' m a@ represents a lazy computation of a value of the type
-- @a@. Unlike regular thunks the @a@ can be computed on any monads @m@
-- implementing 'MonadUnliftIO'.
newtype Deferred m a = Deferred (MVar (Either (m a) a))
-- When the 'MVar' contains 'Left' it means the computation hasn't even
-- started. When it's empty a computation has started but hasn't
-- finished. When it's 'Right' the computation has finished. State
-- transitions are unidirectional: they never go back to previous states.

-- |Create a deferred computation of @a@ to be run on a monad @n@. The
-- monads @m@ and @n@ do not have to be the same.
defer :: MonadIO n => m a -> n (Deferred m a)
defer = (Deferred <$>) . newMVar . Left

-- |Obtain a computed value, or start the computation and wait until it
-- finishes. This function is thread-safe, that is, when two threads
-- concurrently calls 'force' on the same deferred value it's guaranteed
-- at most one of them will compute the value.
force :: MonadUnliftIO m => Deferred m a -> m a
force (Deferred var)
  = do val <- readMVar var
       case val of
         -- readMVar is fast but racy. At the time when we observe 'Left',
         -- another thread may have started the computation so we cannot
         -- trust it yet.
         Left  _ -> modifyMVar var go
         Right a -> pure a
  where
    -- go :: Either (m a) a -> m (Either (m a) a, a)
    go (Right a    ) = pure (Right a, a) -- Race condition detected!
    go (Left  thunk) = thunk >>= \a -> pure (Right a, a)
