{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.ReadOnly
  ( ReadOnlyChan
  , toReadOnlyChan
  ) where

import qualified UnliftIO.Chan as Chan
import Control.Concurrent.Chan.Class

data ReadOnlyChan b = forall a . ReadOnlyChan (Chan.Chan a) (a -> b)

instance Functor ReadOnlyChan where
  fmap f (ReadOnlyChan c f') = ReadOnlyChan c (f . f')

toReadOnlyChan :: Chan.Chan a -> ReadOnlyChan a
toReadOnlyChan c = ReadOnlyChan c id

instance ChanDup ReadOnlyChan where
    dupChan (ReadOnlyChan chan f) = do
      chan' <- Chan.dupChan chan
      return (ReadOnlyChan chan' f)
    {-# INLINE dupChan #-}

instance ChanRead ReadOnlyChan where
    readChan (ReadOnlyChan chan f) = f <$> Chan.readChan chan
    {-# INLINE readChan #-}
