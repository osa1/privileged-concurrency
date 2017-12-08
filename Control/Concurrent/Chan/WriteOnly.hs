{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.WriteOnly
  ( WriteOnlyChan
  , toWriteOnlyChan
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan.Class
import Data.Functor.Contravariant

data WriteOnlyChan a = forall b . WriteOnlyChan (a -> b) (Chan b)

instance Contravariant WriteOnlyChan where
  contramap f (WriteOnlyChan f' c) = WriteOnlyChan (f' . f) c

toWriteOnlyChan :: Chan a -> WriteOnlyChan a
toWriteOnlyChan = WriteOnlyChan id

instance ChanDup WriteOnlyChan where
    dupChan (WriteOnlyChan f chan) = do
      chan' <- dupChan chan
      return (WriteOnlyChan f chan')
    {-# INLINE dupChan #-}

instance ChanWrite WriteOnlyChan where
    writeChan (WriteOnlyChan f chan) = writeChan chan . f
    {-# INLINE writeChan #-}

    writeList2Chan (WriteOnlyChan f chan) = writeList2Chan chan . map f
    {-# INLINE writeList2Chan #-}
