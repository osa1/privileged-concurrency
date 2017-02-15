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

newtype ReadOnlyChan a = ReadOnlyChan (Chan a)

toReadOnlyChan :: Chan a -> ReadOnlyChan a
toReadOnlyChan = ReadOnlyChan

readChan :: MonadBase IO m => ReadOnlyChan a -> m a
readChan (ReadOnlyChan chan) =
  Chan.readChan chan

dupReadOnlyChan :: MonadBase IO m => ReadOnlyChan a -> m (ReadOnlyChan a)
dupReadOnlyChan (ReadOnlyChan chan) = do
  dup <- Chan.dupChan chan
  return (toReadOnlyChan dup)

getChanContents :: MonadBase IO m => ReadOnlyChan a -> m [a]
getChanContents (ReadOnlyChan chan) =
  Chan.getChanContents chan
