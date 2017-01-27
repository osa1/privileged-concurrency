module Control.Concurrent.Chan.WriteOnly 
( WriteOnlyChan
, toWriteOnlyChan
, writeChan
, dupWriteOnlyChan
, writeList2Chan
) where

import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.Chan (Chan)

newtype WriteOnlyChan a = WriteOnlyChan (Chan a)

toWriteOnlyChan :: Chan a -> WriteOnlyChan a
toWriteOnlyChan = WriteOnlyChan

writeChan :: WriteOnlyChan a -> a -> IO ()
writeChan (WriteOnlyChan chan) =
  Chan.writeChan chan
  
dupWriteOnlyChan :: WriteOnlyChan a -> IO (WriteOnlyChan a)
dupWriteOnlyChan (WriteOnlyChan chan) = do
  dup <- Chan.dupChan chan
  return (toWriteOnlyChan dup)
  
writeList2Chan :: WriteOnlyChan a -> [a] -> IO ()
writeList2Chan (WriteOnlyChan chan) =
  Chan.writeList2Chan chan