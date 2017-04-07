{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TVar.WriteOnly
( WriteOnlyTVar
, toWriteOnlyTVar
, writeTVar
) where

import           Control.Concurrent.STM      (STM)
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Functor.Contravariant

data WriteOnlyTVar a = forall b . WriteOnlyTVar (a -> b) (TVar b)

instance Contravariant WriteOnlyTVar where
  contramap f (WriteOnlyTVar f' var) = WriteOnlyTVar (f' . f) var

toWriteOnlyTVar :: TVar a -> WriteOnlyTVar a
toWriteOnlyTVar = WriteOnlyTVar id

writeTVar :: WriteOnlyTVar a -> a -> STM ()
writeTVar (WriteOnlyTVar f var) =
  TVar.writeTVar var . f
