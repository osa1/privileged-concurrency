{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TVar.ReadOnly
( ReadOnlyTVar
, toReadOnlyTVar
, readTVar
, readTVarIO
) where

import           Control.Concurrent.STM      (STM)
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar

data ReadOnlyTVar b = forall a . ReadOnlyTVar (TVar a) (a -> b)

toReadOnlyTVar :: TVar a -> ReadOnlyTVar a
toReadOnlyTVar var = ReadOnlyTVar var id

readTVar :: ReadOnlyTVar a -> STM a
readTVar (ReadOnlyTVar var f) =
  f <$> TVar.readTVar var

readTVarIO :: ReadOnlyTVar a -> IO a
readTVarIO (ReadOnlyTVar var f) =
  f <$> TVar.readTVarIO var
