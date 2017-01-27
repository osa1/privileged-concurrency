module Control.Concurrent.STM.TChan.WriteOnly 
( WriteOnlyTChan
, toWriteOnlyTChan
, writeTChan
, dupWriteOnlyTChan
, unGetTChan
, isEmptyTChan
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TChan (TChan)

newtype WriteOnlyTChan a = WriteOnlyTChan (TChan a)

toWriteOnlyTChan :: TChan a -> WriteOnlyTChan a
toWriteOnlyTChan = WriteOnlyTChan

writeTChan :: WriteOnlyTChan a -> a -> STM ()
writeTChan (WriteOnlyTChan chan) =
  TChan.writeTChan chan

dupWriteOnlyTChan :: WriteOnlyTChan a -> STM (WriteOnlyTChan a)
dupWriteOnlyTChan (WriteOnlyTChan chan) = do
  dup <- TChan.dupTChan chan
  return (toWriteOnlyTChan dup)

unGetTChan :: WriteOnlyTChan a -> a -> STM ()
unGetTChan (WriteOnlyTChan chan) =                
  TChan.unGetTChan chan

isEmptyTChan :: WriteOnlyTChan a -> STM Bool
isEmptyTChan (WriteOnlyTChan chan) =
  TChan.isEmptyTChan chan