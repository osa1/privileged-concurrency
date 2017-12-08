{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TChan.ReadOnly
  ( ReadOnlyTChan
  , toReadOnlyTChan
  ) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TChan.Class

data ReadOnlyTChan b = forall a . ReadOnlyTChan (TChan a) (a -> b)

toReadOnlyTChan :: TChan a -> ReadOnlyTChan a
toReadOnlyTChan chan = ReadOnlyTChan chan id

instance TChanDup ReadOnlyTChan where
    dupTChan (ReadOnlyTChan chan f) = do
      chan' <- dupTChan chan
      return (ReadOnlyTChan chan' f)

instance TChanRead ReadOnlyTChan where
    readTChan (ReadOnlyTChan chan f) = f <$> readTChan chan
    {-# INLINE readTChan #-}

    tryReadTChan (ReadOnlyTChan chan f) = fmap f <$> tryReadTChan chan
    {-# INLINE tryReadTChan #-}

    peekTChan (ReadOnlyTChan chan f) = f <$> peekTChan chan
    {-# INLINE peekTChan #-}

    tryPeekTChan (ReadOnlyTChan chan f) = fmap f <$> tryPeekTChan chan
    {-# INLINE tryPeekTChan #-}
