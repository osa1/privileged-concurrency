module Control.Concurrent.STM.TVar.WriteOnly
( WriteOnlyTVar
, toWriteOnlyTVar
, writeTVar
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar (TVar)

newtype WriteOnlyTVar a = WriteOnlyTVar (TVar a)
    deriving Eq

toWriteOnlyTVar :: TVar a -> WriteOnlyTVar a
toWriteOnlyTVar = WriteOnlyTVar

writeTVar :: WriteOnlyTVar a -> a -> STM ()
writeTVar (WriteOnlyTVar var) =
  TVar.writeTVar var
