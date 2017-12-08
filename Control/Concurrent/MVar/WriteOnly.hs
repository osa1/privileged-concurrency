{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.WriteOnly
  ( WriteOnlyMVar
  , toWriteOnlyMVar
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar.Class
import Data.Functor.Contravariant

data WriteOnlyMVar a = forall b . WriteOnlyMVar (a -> b) (MVar b)

instance Contravariant WriteOnlyMVar where
  contramap f (WriteOnlyMVar f' var) = WriteOnlyMVar (f' . f) var

toWriteOnlyMVar :: MVar a -> WriteOnlyMVar a
toWriteOnlyMVar = WriteOnlyMVar id

instance MVarWrite WriteOnlyMVar where
    putMVar (WriteOnlyMVar f var) a = putMVar var (f a)
    {-# INLINE putMVar #-}

    tryPutMVar (WriteOnlyMVar f var) a = tryPutMVar var (f a)
    {-# INLINE tryPutMVar #-}
