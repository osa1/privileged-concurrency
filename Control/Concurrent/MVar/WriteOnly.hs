module Control.Concurrent.MVar.WriteOnly 
( WriteOnlyMVar
, toWriteOnlyMVar
, putMVar
, tryPutMVar
) where

import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)

newtype WriteOnlyMVar a = WriteOnlyMVar (MVar a)
    deriving Eq

toWriteOnlyMVar :: MVar a -> WriteOnlyMVar a
toWriteOnlyMVar = WriteOnlyMVar

putMVar :: WriteOnlyMVar a -> a -> IO ()
putMVar (WriteOnlyMVar var) =
  MVar.putMVar var

tryPutMVar :: WriteOnlyMVar a -> a -> IO Bool
tryPutMVar (WriteOnlyMVar var) =
  MVar.tryPutMVar var
