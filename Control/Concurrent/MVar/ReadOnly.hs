{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.ReadOnly
  ( ReadOnlyMVar
  , toReadOnlyMVar
  ) where

import Control.Concurrent.MVar.Class
import qualified UnliftIO.MVar as MVar

data ReadOnlyMVar b = forall a . ReadOnlyMVar (MVar.MVar a) (a -> b)

instance Functor ReadOnlyMVar where
  fmap f (ReadOnlyMVar var f') = ReadOnlyMVar var (f . f')

toReadOnlyMVar :: MVar.MVar a -> ReadOnlyMVar a
toReadOnlyMVar var = ReadOnlyMVar var id

instance MVarRead ReadOnlyMVar where
    takeMVar (ReadOnlyMVar var f) = f <$> MVar.takeMVar var
    {-# INLINE takeMVar #-}

    readMVar (ReadOnlyMVar var f) = f <$> MVar.readMVar var
    {-# INLINE readMVar #-}

    tryReadMVar (ReadOnlyMVar var f) = fmap f <$> MVar.tryReadMVar var
    {-# INLINE tryReadMVar #-}

    tryTakeMVar (ReadOnlyMVar var f) = fmap f <$> MVar.tryTakeMVar var
    {-# INLINE tryTakeMVar #-}

    withMVar (ReadOnlyMVar var f) w = MVar.withMVar var (w . f)
    {-# INLINE withMVar #-}
