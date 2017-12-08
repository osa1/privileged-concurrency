{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TChan.WriteOnly
  ( WriteOnlyTChan
  , toWriteOnlyTChan
  ) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TChan.Class
import Data.Functor.Contravariant

data WriteOnlyTChan a = forall b . WriteOnlyTChan (a -> b) (TChan b)

instance Contravariant WriteOnlyTChan where
  contramap f (WriteOnlyTChan f' chan) = WriteOnlyTChan (f' . f) chan

toWriteOnlyTChan :: TChan a -> WriteOnlyTChan a
toWriteOnlyTChan = WriteOnlyTChan id

instance TChanDup WriteOnlyTChan where
    dupTChan (WriteOnlyTChan f chan) = WriteOnlyTChan f <$> dupTChan chan

instance TChanWrite WriteOnlyTChan where
    writeTChan (WriteOnlyTChan f chan) = writeTChan chan . f
    {-# INLINE writeTChan #-}

    unGetTChan (WriteOnlyTChan f chan) = unGetTChan chan . f
    {-# INLINE unGetTChan #-}

    isEmptyTChan (WriteOnlyTChan _ chan) = isEmptyTChan chan
    {-# INLINE isEmptyTChan #-}
