module Control.Concurrent.STM.TMVar.WriteOnly
( WriteOnlyTMVar
, toWriteOnlyTMVar
, putTMVar
, tryPutTMVar
, isEmptyWriteOnlyTMVar
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TMVar as TMVar
import Control.Concurrent.STM.TMVar (TMVar)

newtype WriteOnlyTMVar a = WriteOnlyTMVar (TMVar a)
    deriving Eq

toWriteOnlyTMVar :: TMVar a -> WriteOnlyTMVar a
toWriteOnlyTMVar = WriteOnlyTMVar

putTMVar :: WriteOnlyTMVar a -> a -> STM ()
putTMVar (WriteOnlyTMVar var) =
  TMVar.putTMVar var

tryPutTMVar :: WriteOnlyTMVar a -> a -> STM Bool
tryPutTMVar (WriteOnlyTMVar var) =
  TMVar.tryPutTMVar var

isEmptyWriteOnlyTMVar :: (WriteOnlyTMVar a) -> STM Bool
isEmptyWriteOnlyTMVar (WriteOnlyTMVar var) =
  TMVar.isEmptyTMVar var