{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TMVar.WriteOnly
  ( WriteOnlyTMVar
  , toWriteOnlyTMVar
  ) where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TMVar.Class
import Data.Functor.Contravariant

data WriteOnlyTMVar a = forall b . WriteOnlyTMVar (a -> b) (TMVar b)

instance Contravariant WriteOnlyTMVar where
  contramap f (WriteOnlyTMVar f' var) = WriteOnlyTMVar (f' . f) var

toWriteOnlyTMVar :: TMVar a -> WriteOnlyTMVar a
toWriteOnlyTMVar = WriteOnlyTMVar id

instance TMVarWrite WriteOnlyTMVar where
    putTMVar (WriteOnlyTMVar f var) = putTMVar var . f
    {-# INLINE putTMVar #-}

    tryPutTMVar (WriteOnlyTMVar f var) = tryPutTMVar var . f
    {-# INLINE tryPutTMVar #-}
