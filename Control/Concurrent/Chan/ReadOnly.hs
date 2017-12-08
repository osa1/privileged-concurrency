{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.ReadOnly
  ( ReadOnlyChan
  , toReadOnlyChan
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan.Class

data ReadOnlyChan b = forall a . ReadOnlyChan (Chan a) (a -> b)

instance Functor ReadOnlyChan where
  fmap f (ReadOnlyChan c f') = ReadOnlyChan c (f . f')

toReadOnlyChan :: Chan a -> ReadOnlyChan a
toReadOnlyChan c = ReadOnlyChan c id

instance ChanDup ReadOnlyChan where
    dupChan (ReadOnlyChan chan f) = do
      chan' <- dupChan chan
      return (ReadOnlyChan chan' f)
    {-# INLINE dupChan #-}

instance ChanRead ReadOnlyChan where
    readChan (ReadOnlyChan chan f) = f <$> readChan chan
    {-# INLINE readChan #-}
