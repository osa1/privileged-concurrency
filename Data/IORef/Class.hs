module Data.IORef.Class where

import Control.Monad.IO.Class
import qualified UnliftIO.IORef as IORef

class IORefRead var where
  readIORef :: MonadIO m => var a -> m a

instance IORefRead IORef.IORef where
  readIORef = IORef.readIORef
  {-# INLINE readIORef #-}

class IORefWrite var where
  writeIORef :: MonadIO m => var a -> a -> m ()
  atomicWriteIORef :: MonadIO m => var a -> a -> m ()

instance IORefWrite IORef.IORef where
  writeIORef = IORef.writeIORef
  {-# INLINE writeIORef #-}

  atomicWriteIORef = IORef.atomicWriteIORef
  {-# INLINE atomicWriteIORef #-}

class (IORefRead var, IORefWrite var) => IORefReadWrite var where
  modifyIORef :: MonadIO m => var a -> (a -> a) -> m ()
  modifyIORef' :: MonadIO m => var a -> (a -> a) -> m ()
  atomicModifyIORef :: MonadIO m => var a -> (a -> (a, b)) -> m b
  atomicModifyIORef' :: MonadIO m => var a -> (a -> (a, b)) -> m b

instance IORefReadWrite IORef.IORef where
  modifyIORef = IORef.modifyIORef
  {-# INLINE modifyIORef #-}

  modifyIORef' = IORef.modifyIORef'
  {-# INLINE modifyIORef' #-}

  atomicModifyIORef = IORef.atomicModifyIORef
  {-# INLINE atomicModifyIORef #-}

  atomicModifyIORef' = IORef.atomicModifyIORef'
  {-# INLINE atomicModifyIORef' #-}
