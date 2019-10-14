{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.ReadOnly
  ( ReadOnlyMVar
  , toReadOnlyMVar
  ) where

import           Control.Concurrent.MVar       (MVar)
import           Control.Concurrent.MVar.Class

data ReadOnlyMVar b = forall a . ReadOnlyMVar (MVar a) (a -> b)

instance Functor ReadOnlyMVar where
  fmap f (ReadOnlyMVar var f') = ReadOnlyMVar var (f . f')

toReadOnlyMVar :: MVar a -> ReadOnlyMVar a
toReadOnlyMVar var = ReadOnlyMVar var id

instance MVarRead ReadOnlyMVar where
    takeMVar (ReadOnlyMVar var f) = f <$> takeMVar var
    {-# INLINE takeMVar #-}

    readMVar (ReadOnlyMVar var f) = f <$> readMVar var
    {-# INLINE readMVar #-}

    tryReadMVar (ReadOnlyMVar var f) = fmap f <$> tryReadMVar var
    {-# INLINE tryReadMVar #-}

    tryTakeMVar (ReadOnlyMVar var f) = fmap f <$> tryTakeMVar var
    {-# INLINE tryTakeMVar #-}

    withMVar (ReadOnlyMVar var f) w = withMVar var (w . f)
    {-# INLINE withMVar #-}
