{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TChan.ReadOnly
( ReadOnlyTChan
, toReadOnlyTChan
, readTChan
, dupReadOnlyTChan
) where

import           Control.Concurrent.STM       (STM)
import           Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan

data ReadOnlyTChan b = forall a . ReadOnlyTChan (TChan a) (a -> b)

toReadOnlyTChan :: TChan a -> ReadOnlyTChan a
toReadOnlyTChan chan = ReadOnlyTChan chan id

readTChan :: ReadOnlyTChan a -> STM a
readTChan (ReadOnlyTChan chan f) =
  f <$> TChan.readTChan chan

dupReadOnlyTChan :: ReadOnlyTChan a -> STM (ReadOnlyTChan a)
dupReadOnlyTChan (ReadOnlyTChan chan f) = do
  dup <- TChan.dupTChan chan
  return (ReadOnlyTChan dup f)
