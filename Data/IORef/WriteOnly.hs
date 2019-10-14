{-# LANGUAGE ExistentialQuantification #-}

module Data.IORef.WriteOnly
  ( WriteOnlyIORef
  , toWriteOnlyIORef
  ) where

import Data.Functor.Contravariant
import Data.IORef (IORef)
import Data.IORef.Class

data WriteOnlyIORef a = forall b . WriteOnlyIORef (a -> b) (IORef b)

instance Contravariant WriteOnlyIORef where
  contramap f (WriteOnlyIORef f' ref) = WriteOnlyIORef (f' . f) ref

toWriteOnlyIORef :: IORef a -> WriteOnlyIORef a
toWriteOnlyIORef = WriteOnlyIORef id

instance IORefWrite WriteOnlyIORef where
  writeIORef (WriteOnlyIORef f ref) a = writeIORef ref (f a)
  {-# INLINE writeIORef #-}

  atomicWriteIORef (WriteOnlyIORef f ref) a = atomicWriteIORef ref (f a)
  {-# INLINE atomicWriteIORef #-}
