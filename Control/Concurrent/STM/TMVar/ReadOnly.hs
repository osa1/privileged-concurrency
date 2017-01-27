module Control.Concurrent.STM.TMVar.ReadOnly
( ReadOnlyTMVar
, toReadOnlyTMVar
, takeTMVar
, readTMVar
, tryTakeTMVar
, isEmptyTMVar
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TMVar as TMVar
import Control.Concurrent.STM.TMVar (TMVar)

newtype ReadOnlyTMVar a = ReadOnlyTMVar (TMVar a)
                      deriving Eq

toReadOnlyTMVar :: TMVar a -> ReadOnlyTMVar a
toReadOnlyTMVar = ReadOnlyTMVar

takeTMVar :: ReadOnlyTMVar a -> STM a
takeTMVar (ReadOnlyTMVar var) =
  TMVar.takeTMVar var

readTMVar :: ReadOnlyTMVar a -> STM a
readTMVar (ReadOnlyTMVar var) =
  TMVar.readTMVar var

tryTakeTMVar :: ReadOnlyTMVar a -> STM (Maybe a)
tryTakeTMVar (ReadOnlyTMVar var) =
  TMVar.tryTakeTMVar var
  
isEmptyTMVar :: (ReadOnlyTMVar a) -> STM Bool
isEmptyTMVar (ReadOnlyTMVar var) =
  TMVar.isEmptyTMVar var