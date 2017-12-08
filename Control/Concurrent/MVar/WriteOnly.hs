{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.MVar.WriteOnly
  ( WriteOnlyMVar
  , toWriteOnlyMVar
  ) where

import Control.Concurrent.MVar.Class
import Data.Functor.Contravariant
import qualified UnliftIO.MVar as MVar

data WriteOnlyMVar a = forall b . WriteOnlyMVar (a -> b) (MVar.MVar b)

instance Contravariant WriteOnlyMVar where
  contramap f (WriteOnlyMVar f' var) = WriteOnlyMVar (f' . f) var

toWriteOnlyMVar :: MVar.MVar a -> WriteOnlyMVar a
toWriteOnlyMVar = WriteOnlyMVar id

instance MVarWrite WriteOnlyMVar where
    putMVar (WriteOnlyMVar f var) a = MVar.putMVar var (f a)
    {-# INLINE putMVar #-}

    tryPutMVar (WriteOnlyMVar f var) a = MVar.tryPutMVar var (f a)
    {-# INLINE tryPutMVar #-}
