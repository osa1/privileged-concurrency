module Control.Concurrent.Chan.WriteOnly
( WriteOnlyChan
, toWriteOnlyChan
, writeChan
, dupWriteOnlyChan
, writeList2Chan
) where

import           Control.Concurrent.Chan.Lifted (Chan)
import qualified Control.Concurrent.Chan.Lifted as Chan
import           Control.Monad.Base

newtype WriteOnlyChan a = WriteOnlyChan (Chan a)

toWriteOnlyChan :: Chan a -> WriteOnlyChan a
toWriteOnlyChan = WriteOnlyChan

writeChan :: MonadBase IO m => WriteOnlyChan a -> a -> m ()
writeChan (WriteOnlyChan chan) =
  Chan.writeChan chan

dupWriteOnlyChan :: MonadBase IO m => WriteOnlyChan a -> m (WriteOnlyChan a)
dupWriteOnlyChan (WriteOnlyChan chan) = do
  dup <- Chan.dupChan chan
  return (toWriteOnlyChan dup)

writeList2Chan :: MonadBase IO m => WriteOnlyChan a -> [a] -> m ()
writeList2Chan (WriteOnlyChan chan) =
  Chan.writeList2Chan chan
