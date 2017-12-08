module Control.Concurrent.STM.TVar.Class where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TVar as TVar

class TVarWrite var where
    writeTVar :: var a -> a -> STM ()

instance TVarWrite TVar.TVar where
    writeTVar = TVar.writeTVar
    {-# INLINE writeTVar #-}

class TVarRead var where
    readTVar :: var a -> STM a

instance TVarRead TVar.TVar where
    readTVar = TVar.readTVar
    {-# INLINE readTVar #-}
