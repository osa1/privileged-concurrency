{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.WriteOnly
( WriteOnlyChan
, toWriteOnlyChan
, writeChan
, dupWriteOnlyChan
, writeList2Chan
) where

import           Control.Concurrent.Chan.Lifted (Chan)
import qualified Control.Concurrent.Chan.Lifted as Chan
import           Control.Monad.Base
import           Data.Functor.Contravariant

data WriteOnlyChan a = forall b . WriteOnlyChan (a -> b) (Chan b)

instance Contravariant WriteOnlyChan where
  contramap f (WriteOnlyChan f' c) = WriteOnlyChan (f' . f) c

toWriteOnlyChan :: Chan a -> WriteOnlyChan a
toWriteOnlyChan = WriteOnlyChan id

writeChan :: MonadBase IO m => WriteOnlyChan a -> a -> m ()
writeChan (WriteOnlyChan f chan) =
  Chan.writeChan chan . f

dupWriteOnlyChan :: MonadBase IO m => WriteOnlyChan a -> m (WriteOnlyChan a)
dupWriteOnlyChan (WriteOnlyChan f chan) = do
  dup <- Chan.dupChan chan
  return (WriteOnlyChan f dup)

writeList2Chan :: MonadBase IO m => WriteOnlyChan a -> [a] -> m ()
writeList2Chan (WriteOnlyChan f chan) =
  Chan.writeList2Chan chan . map f
