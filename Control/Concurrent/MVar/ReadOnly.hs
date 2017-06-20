{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.ReadOnly
( ReadOnlyMVar
, toReadOnlyMVar
, takeMVar
, readMVar
, tryReadMVar
, tryTakeMVar
, withMVar
) where

import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.Monad.Base
import           Control.Monad.Trans.Control    (MonadBaseControl)

data ReadOnlyMVar b = forall a . ReadOnlyMVar (MVar a) (a -> b)

instance Functor ReadOnlyMVar where
  fmap f (ReadOnlyMVar var f') = ReadOnlyMVar var (f . f')

toReadOnlyMVar :: MVar a -> ReadOnlyMVar a
toReadOnlyMVar var = ReadOnlyMVar var id

takeMVar :: MonadBase IO m => ReadOnlyMVar a -> m a
takeMVar (ReadOnlyMVar var f) =
  f <$> MVar.takeMVar var

readMVar :: MonadBase IO m => ReadOnlyMVar a -> m a
readMVar (ReadOnlyMVar var f) =
  f <$> MVar.readMVar var

tryReadMVar :: MonadBase IO m => ReadOnlyMVar a -> m (Maybe a)
tryReadMVar (ReadOnlyMVar var f) =
  fmap f <$> MVar.tryReadMVar var

tryTakeMVar :: MonadBase IO m => ReadOnlyMVar a -> m (Maybe a)
tryTakeMVar (ReadOnlyMVar var f) =
  fmap f <$> MVar.tryTakeMVar var

withMVar :: MonadBaseControl IO m => ReadOnlyMVar a -> (a -> m b) -> m b
withMVar (ReadOnlyMVar var f) w =
  MVar.withMVar var $ \var' -> w (f var')
