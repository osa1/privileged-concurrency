{-# LANGUAGE ExistentialQuantification #-}

module Data.IORef.ReadWrite
  ( ReadWriteIORef
  , toReadWriteIORef
  ) where

import Data.Bifunctor (first)
import Data.IORef (IORef)
import Data.IORef.Class

data ReadWriteIORef a = forall b. ReadWriteIORef (IORef b) (a -> b) (b -> a)

toReadWriteIORef :: IORef a -> ReadWriteIORef a
toReadWriteIORef ref = ReadWriteIORef ref id id

instance IORefRead ReadWriteIORef where
  readIORef (ReadWriteIORef ref _ g) = g <$> readIORef ref
  {-# INLINE readIORef #-}

instance IORefWrite ReadWriteIORef where
  writeIORef (ReadWriteIORef ref f _) a = writeIORef ref (f a)
  {-# INLINE writeIORef #-}

  atomicWriteIORef (ReadWriteIORef ref f _) a = atomicWriteIORef ref (f a)
  {-# INLINE atomicWriteIORef #-}

instance IORefReadWrite ReadWriteIORef where
  modifyIORef (ReadWriteIORef ref f g) h = modifyIORef ref (f . h . g)
  {-# INLINE modifyIORef #-}

  modifyIORef' (ReadWriteIORef ref f g) h = modifyIORef' ref (f . h . g)
  {-# INLINE modifyIORef' #-}

  atomicModifyIORef (ReadWriteIORef ref f g) h = atomicModifyIORef ref $ first f . h . g
  {-# INLINE atomicModifyIORef #-}

  atomicModifyIORef' (ReadWriteIORef ref f g) h = atomicModifyIORef' ref $ first f . h . g
  {-# INLINE atomicModifyIORef' #-}
