{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TMVar.ReadOnly
( ReadOnlyTMVar
, toReadOnlyTMVar
, takeTMVar
, readTMVar
, tryTakeTMVar
, isEmptyTMVar
) where

import           Control.Concurrent.STM       (STM)
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar

data ReadOnlyTMVar b = forall a . ReadOnlyTMVar (TMVar a) (a -> b)

toReadOnlyTMVar :: TMVar a -> ReadOnlyTMVar a
toReadOnlyTMVar var = ReadOnlyTMVar var id

takeTMVar :: ReadOnlyTMVar a -> STM a
takeTMVar (ReadOnlyTMVar var f) =
  f <$> TMVar.takeTMVar var

readTMVar :: ReadOnlyTMVar a -> STM a
readTMVar (ReadOnlyTMVar var f) =
  f <$> TMVar.readTMVar var

tryTakeTMVar :: ReadOnlyTMVar a -> STM (Maybe a)
tryTakeTMVar (ReadOnlyTMVar var f) =
  fmap f <$> TMVar.tryTakeTMVar var

isEmptyTMVar :: ReadOnlyTMVar a -> STM Bool
isEmptyTMVar (ReadOnlyTMVar var _) =
  TMVar.isEmptyTMVar var
