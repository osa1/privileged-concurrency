module Control.Concurrent.Chan.ReadOnly
( ReadOnlyChan
, toReadOnlyChan
, readChan
, dupReadOnlyChan
, getChanContents
) where

import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.Chan (Chan)

newtype ReadOnlyChan a = ReadOnlyChan (Chan a)

toReadOnlyChan :: Chan a -> ReadOnlyChan a
toReadOnlyChan = ReadOnlyChan

readChan :: ReadOnlyChan a -> IO a
readChan (ReadOnlyChan chan) =
  Chan.readChan chan
  
dupReadOnlyChan :: ReadOnlyChan a -> IO (ReadOnlyChan a)
dupReadOnlyChan (ReadOnlyChan chan) = do
  dup <- Chan.dupChan chan
  return (toReadOnlyChan dup)

getChanContents :: ReadOnlyChan a -> IO [a]
getChanContents (ReadOnlyChan chan) =
  Chan.getChanContents chan