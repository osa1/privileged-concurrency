{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TMVar.ReadOnly
  ( ReadOnlyTMVar
  , toReadOnlyTMVar
  ) where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TMVar.Class

data ReadOnlyTMVar b = forall a . ReadOnlyTMVar (TMVar a) (a -> b)

toReadOnlyTMVar :: TMVar a -> ReadOnlyTMVar a
toReadOnlyTMVar var = ReadOnlyTMVar var id

instance TMVarRead ReadOnlyTMVar where
    takeTMVar (ReadOnlyTMVar var f) = f <$> takeTMVar var
    {-# INLINE takeTMVar #-}

    readTMVar (ReadOnlyTMVar var f) = f <$> readTMVar var
    {-# INLINE readTMVar #-}

    tryTakeTMVar (ReadOnlyTMVar var f) = fmap f <$> tryTakeTMVar var
    {-# INLINE tryTakeTMVar #-}
