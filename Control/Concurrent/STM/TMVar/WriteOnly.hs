{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TMVar.WriteOnly
( WriteOnlyTMVar
, toWriteOnlyTMVar
, putTMVar
, tryPutTMVar
, isEmptyWriteOnlyTMVar
) where

import           Control.Concurrent.STM       (STM)
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Data.Functor.Contravariant

data WriteOnlyTMVar a = forall b . WriteOnlyTMVar (a -> b) (TMVar b)

instance Contravariant WriteOnlyTMVar where
  contramap f (WriteOnlyTMVar f' var) = WriteOnlyTMVar (f' . f) var

toWriteOnlyTMVar :: TMVar a -> WriteOnlyTMVar a
toWriteOnlyTMVar = WriteOnlyTMVar id

putTMVar :: WriteOnlyTMVar a -> a -> STM ()
putTMVar (WriteOnlyTMVar f var) =
  TMVar.putTMVar var . f

tryPutTMVar :: WriteOnlyTMVar a -> a -> STM Bool
tryPutTMVar (WriteOnlyTMVar f var) =
  TMVar.tryPutTMVar var . f

isEmptyWriteOnlyTMVar :: WriteOnlyTMVar a -> STM Bool
isEmptyWriteOnlyTMVar (WriteOnlyTMVar _ var) =
  TMVar.isEmptyTMVar var
