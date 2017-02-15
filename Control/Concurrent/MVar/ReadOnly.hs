module Control.Concurrent.MVar.ReadOnly
( ReadOnlyMVar
, toReadOnlyMVar
, takeMVar
, readMVar
, tryTakeMVar
, withMVar
) where

import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.Monad.Base
import           Control.Monad.Trans.Control    (MonadBaseControl)

newtype ReadOnlyMVar a = ReadOnlyMVar (MVar a)
    deriving Eq

toReadOnlyMVar :: MVar a -> ReadOnlyMVar a
toReadOnlyMVar = ReadOnlyMVar

takeMVar :: MonadBase IO m => ReadOnlyMVar a -> m a
takeMVar (ReadOnlyMVar var) =
  MVar.takeMVar var

readMVar :: MonadBase IO m => ReadOnlyMVar a -> m a
readMVar (ReadOnlyMVar var) =
  MVar.readMVar var

tryTakeMVar :: MonadBase IO m => ReadOnlyMVar a -> m (Maybe a)
tryTakeMVar (ReadOnlyMVar var) =
  MVar.tryTakeMVar var

withMVar :: MonadBaseControl IO m => ReadOnlyMVar a -> (a -> m b) -> m b
withMVar (ReadOnlyMVar var) =
  MVar.withMVar var
