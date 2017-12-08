{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TVar.WriteOnly
  ( WriteOnlyTVar
  , toWriteOnlyTVar
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar.Class
import Data.Functor.Contravariant

data WriteOnlyTVar a = forall b . WriteOnlyTVar (a -> b) (TVar b)

instance Contravariant WriteOnlyTVar where
  contramap f (WriteOnlyTVar f' var) = WriteOnlyTVar (f' . f) var

toWriteOnlyTVar :: TVar a -> WriteOnlyTVar a
toWriteOnlyTVar = WriteOnlyTVar id

instance TVarWrite WriteOnlyTVar where
    writeTVar (WriteOnlyTVar f var) = writeTVar var . f
    {-# INLINE writeTVar #-}
