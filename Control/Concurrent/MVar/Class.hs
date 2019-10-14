module Control.Concurrent.MVar.Class where

import           Control.Monad.IO.Unlift
import qualified UnliftIO.MVar           as MVar

class MVarWrite var where
    putMVar :: MonadIO m => var a -> a -> m ()
    tryPutMVar :: MonadIO m => var a -> a -> m Bool

instance MVarWrite MVar.MVar where
    putMVar = MVar.putMVar
    {-# INLINE putMVar #-}

    tryPutMVar = MVar.tryPutMVar
    {-# INLINE tryPutMVar #-}

class MVarRead var where
    takeMVar :: MonadIO m => var a -> m a
    readMVar :: MonadIO m => var a -> m a
    tryReadMVar :: MonadIO m => var a -> m (Maybe a)
    tryTakeMVar :: MonadIO m => var a -> m (Maybe a)
    withMVar :: MonadUnliftIO m => var a -> (a -> m b) -> m b

instance MVarRead MVar.MVar where
    takeMVar = MVar.takeMVar
    {-# INLINE takeMVar #-}

    readMVar = MVar.readMVar
    {-# INLINE readMVar #-}

    tryReadMVar = MVar.tryReadMVar
    {-# INLINE tryReadMVar #-}

    tryTakeMVar = MVar.tryTakeMVar
    {-# INLINE tryTakeMVar #-}

    withMVar = MVar.withMVar
    {-# INLINE withMVar #-}
