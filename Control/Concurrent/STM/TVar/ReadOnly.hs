{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TVar.ReadOnly
  ( ReadOnlyTVar
  , toReadOnlyTVar
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar.Class

data ReadOnlyTVar b = forall a . ReadOnlyTVar (TVar a) (a -> b)

toReadOnlyTVar :: TVar a -> ReadOnlyTVar a
toReadOnlyTVar var = ReadOnlyTVar var id

instance TVarRead ReadOnlyTVar where
    readTVar (ReadOnlyTVar var f) = f <$> readTVar var
    {-# INLINE readTVar #-}
