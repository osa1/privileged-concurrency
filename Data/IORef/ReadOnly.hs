{-# LANGUAGE ExistentialQuantification #-}

module Data.IORef.ReadOnly
  ( ReadOnlyIORef
  , toReadOnlyIORef
  ) where

import           Data.IORef       (IORef)
import           Data.IORef.Class

data ReadOnlyIORef b = forall a. ReadOnlyIORef (IORef a) (a -> b)

instance Functor ReadOnlyIORef where
  fmap f (ReadOnlyIORef ref f') = ReadOnlyIORef ref (f . f')

toReadOnlyIORef :: IORef a -> ReadOnlyIORef a
toReadOnlyIORef ref = ReadOnlyIORef ref id

instance IORefRead ReadOnlyIORef where
  readIORef (ReadOnlyIORef ref f) = f <$> readIORef ref
  {-# INLINE readIORef #-}
