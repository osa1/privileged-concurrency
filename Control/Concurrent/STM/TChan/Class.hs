module Control.Concurrent.STM.TChan.Class where

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TChan as TChan

class TChanDup chan where
    dupTChan :: chan a -> STM (chan a)

instance TChanDup TChan.TChan where
    dupTChan = TChan.dupTChan
    {-# INLINE dupTChan #-}

class TChanWrite chan where
    writeTChan :: chan a -> a -> STM ()
    unGetTChan :: chan a -> a -> STM ()
    isEmptyTChan :: chan a -> STM Bool

instance TChanWrite TChan.TChan where
    writeTChan = TChan.writeTChan
    {-# INLINE writeTChan #-}

    unGetTChan = TChan.unGetTChan
    {-# INLINE unGetTChan #-}

    isEmptyTChan = TChan.isEmptyTChan
    {-# INLINE isEmptyTChan #-}

class TChanRead chan where
    readTChan :: chan a -> STM a
    tryReadTChan :: chan a -> STM (Maybe a)
    peekTChan :: chan a -> STM a
    tryPeekTChan :: chan a -> STM (Maybe a)

instance TChanRead TChan.TChan where
    readTChan = TChan.readTChan
    {-# INLINE readTChan #-}

    tryReadTChan = TChan.tryReadTChan
    {-# INLINE tryReadTChan #-}

    peekTChan = TChan.peekTChan
    {-# INLINE peekTChan #-}

    tryPeekTChan = TChan.tryPeekTChan
    {-# INLINE tryPeekTChan #-}
