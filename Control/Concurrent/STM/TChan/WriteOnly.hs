{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.STM.TChan.WriteOnly
( WriteOnlyTChan
, toWriteOnlyTChan
, writeTChan
, dupWriteOnlyTChan
, unGetTChan
, isEmptyTChan
) where

import           Control.Concurrent.STM       (STM)
import           Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import           Data.Functor.Contravariant

data WriteOnlyTChan a = forall b . WriteOnlyTChan (a -> b) (TChan b)

instance Contravariant WriteOnlyTChan where
  contramap f (WriteOnlyTChan f' chan) = WriteOnlyTChan (f' . f) chan

toWriteOnlyTChan :: TChan a -> WriteOnlyTChan a
toWriteOnlyTChan = WriteOnlyTChan id

writeTChan :: WriteOnlyTChan a -> a -> STM ()
writeTChan (WriteOnlyTChan f chan) =
  TChan.writeTChan chan . f

dupWriteOnlyTChan :: WriteOnlyTChan a -> STM (WriteOnlyTChan a)
dupWriteOnlyTChan (WriteOnlyTChan f chan) = do
  dup <- TChan.dupTChan chan
  return (WriteOnlyTChan f dup)

unGetTChan :: WriteOnlyTChan a -> a -> STM ()
unGetTChan (WriteOnlyTChan f chan) =
  TChan.unGetTChan chan . f

isEmptyTChan :: WriteOnlyTChan a -> STM Bool
isEmptyTChan (WriteOnlyTChan _ chan) =
  TChan.isEmptyTChan chan
