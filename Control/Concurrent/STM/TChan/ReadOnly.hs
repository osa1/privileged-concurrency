module Control.Concurrent.STM.TChan.ReadOnly
( ReadOnlyTChan
, toReadOnlyTChan
, readTChan
, dupReadOnlyTChan
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TChan (TChan)

newtype ReadOnlyTChan a = ReadOnlyTChan (TChan a)
                     
toReadOnlyTChan :: TChan a -> ReadOnlyTChan a
toReadOnlyTChan = ReadOnlyTChan

readTChan :: ReadOnlyTChan a -> STM a
readTChan (ReadOnlyTChan chan) =
  TChan.readTChan chan

dupReadOnlyTChan :: ReadOnlyTChan a -> STM (ReadOnlyTChan a)
dupReadOnlyTChan (ReadOnlyTChan chan) = do
  dup <- TChan.dupTChan chan
  return (toReadOnlyTChan dup)
