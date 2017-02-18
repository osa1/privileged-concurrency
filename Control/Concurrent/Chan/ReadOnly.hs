{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.ReadOnly
( ReadOnlyChan
, toReadOnlyChan
, readChan
, dupReadOnlyChan
, getChanContents
) where

import           Control.Concurrent.Chan.Lifted (Chan)
import qualified Control.Concurrent.Chan.Lifted as Chan
import           Control.Monad.Base

data ReadOnlyChan b = forall a . ReadOnlyChan (Chan a) (a -> b)

instance Functor ReadOnlyChan where
  fmap f (ReadOnlyChan c f') = ReadOnlyChan c (f . f')

toReadOnlyChan :: Chan a -> ReadOnlyChan a
toReadOnlyChan c = ReadOnlyChan c id

readChan :: MonadBase IO m => ReadOnlyChan a -> m a
readChan (ReadOnlyChan chan f) =
  f <$> Chan.readChan chan

dupReadOnlyChan :: MonadBase IO m => ReadOnlyChan a -> m (ReadOnlyChan a)
dupReadOnlyChan (ReadOnlyChan chan f) = do
  dup <- Chan.dupChan chan
  return (ReadOnlyChan dup f)

getChanContents :: MonadBase IO m => ReadOnlyChan a -> m [a]
getChanContents (ReadOnlyChan chan f) =
  fmap f <$> Chan.getChanContents chan
