{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.WriteOnly
( WriteOnlyMVar
, toWriteOnlyMVar
, putMVar
, tryPutMVar
) where

import           Control.Concurrent.MVar.Lifted (MVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import           Control.Monad.Base
import           Data.Functor.Contravariant

data WriteOnlyMVar a = forall b . WriteOnlyMVar (a -> b) (MVar b)

instance Contravariant WriteOnlyMVar where
  contramap f (WriteOnlyMVar f' var) = WriteOnlyMVar (f' . f) var

toWriteOnlyMVar :: MVar a -> WriteOnlyMVar a
toWriteOnlyMVar = WriteOnlyMVar id

putMVar :: MonadBase IO m => WriteOnlyMVar a -> a -> m ()
putMVar (WriteOnlyMVar f var) =
  MVar.putMVar var . f

tryPutMVar :: MonadBase IO m => WriteOnlyMVar a -> a -> m Bool
tryPutMVar (WriteOnlyMVar f var) =
  MVar.tryPutMVar var . f
