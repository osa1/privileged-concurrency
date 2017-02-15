module Control.Concurrent.MVar.WriteOnly
( WriteOnlyMVar
, toWriteOnlyMVar
, putMVar
, tryPutMVar
) where

import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.Monad.Base

newtype WriteOnlyMVar a = WriteOnlyMVar (MVar a)
    deriving Eq

toWriteOnlyMVar :: MVar a -> WriteOnlyMVar a
toWriteOnlyMVar = WriteOnlyMVar

putMVar :: MonadBase IO m => WriteOnlyMVar a -> a -> m ()
putMVar (WriteOnlyMVar var) =
  MVar.putMVar var

tryPutMVar :: MonadBase IO m => WriteOnlyMVar a -> a -> m Bool
tryPutMVar (WriteOnlyMVar var) =
  MVar.tryPutMVar var
