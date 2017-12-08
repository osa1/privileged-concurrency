module Control.Concurrent.STM.TMVar.Class where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TMVar as TMVar

class TMVarWrite var where
    putTMVar :: var a -> a -> STM ()
    tryPutTMVar :: var a -> a -> STM Bool

instance TMVarWrite TMVar.TMVar where
    putTMVar = TMVar.putTMVar
    {-# INLINE putTMVar #-}

    tryPutTMVar = TMVar.tryPutTMVar
    {-# INLINE tryPutTMVar #-}

class TMVarRead var where
    takeTMVar :: var a -> STM a
    readTMVar :: var a -> STM a
    tryTakeTMVar :: var a -> STM (Maybe a)

instance TMVarRead TMVar.TMVar where
    takeTMVar = TMVar.takeTMVar
    {-# INLINE takeTMVar #-}

    readTMVar = TMVar.readTMVar
    {-# INLINE readTMVar #-}

    tryTakeTMVar = TMVar.tryTakeTMVar
    {-# INLINE tryTakeTMVar #-}
