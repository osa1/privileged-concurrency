module Control.Concurrent.STM.TVar.ReadOnly
( ReadOnlyTVar
, toReadOnlyTVar
, readTVar
, readTVarIO
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar (TVar)

newtype ReadOnlyTVar a = ReadOnlyTVar (TVar a)
    deriving Eq

toReadOnlyTVar :: TVar a -> ReadOnlyTVar a
toReadOnlyTVar = ReadOnlyTVar

readTVar :: ReadOnlyTVar a -> STM a
readTVar (ReadOnlyTVar var) =
  TVar.readTVar var
  
readTVarIO :: ReadOnlyTVar a -> IO a
readTVarIO (ReadOnlyTVar var) =
  TVar.readTVarIO var
