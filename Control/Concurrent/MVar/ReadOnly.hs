module Control.Concurrent.MVar.ReadOnly
( ReadOnlyMVar
, toReadOnlyMVar
, takeMVar
, readMVar
, tryTakeMVar
, withMVar
) where

import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)

newtype ReadOnlyMVar a = ReadOnlyMVar (MVar a)
    deriving Eq

toReadOnlyMVar :: MVar a -> ReadOnlyMVar a
toReadOnlyMVar = ReadOnlyMVar

takeMVar :: ReadOnlyMVar a -> IO a
takeMVar (ReadOnlyMVar var) =
  MVar.takeMVar var

readMVar :: ReadOnlyMVar a -> IO a
readMVar (ReadOnlyMVar var) =
  MVar.readMVar var

tryTakeMVar :: ReadOnlyMVar a -> IO (Maybe a)
tryTakeMVar (ReadOnlyMVar var) =
  MVar.tryTakeMVar var

withMVar :: ReadOnlyMVar a -> (a -> IO b) -> IO b
withMVar (ReadOnlyMVar var) =
  MVar.withMVar var