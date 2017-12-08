{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Chan.WriteOnly
  ( WriteOnlyChan
  , toWriteOnlyChan
  ) where

import Control.Concurrent.Chan.Class
import Data.Functor.Contravariant
import qualified UnliftIO.Chan as Chan

data WriteOnlyChan a = forall b . WriteOnlyChan (a -> b) (Chan.Chan b)

instance Contravariant WriteOnlyChan where
  contramap f (WriteOnlyChan f' c) = WriteOnlyChan (f' . f) c

toWriteOnlyChan :: Chan.Chan a -> WriteOnlyChan a
toWriteOnlyChan = WriteOnlyChan id

instance ChanDup WriteOnlyChan where
    dupChan (WriteOnlyChan f chan) = do
      chan' <- Chan.dupChan chan
      return (WriteOnlyChan f chan')
    {-# INLINE dupChan #-}

instance ChanWrite WriteOnlyChan where
    writeChan (WriteOnlyChan f chan) = Chan.writeChan chan . f
    {-# INLINE writeChan #-}

    writeList2Chan (WriteOnlyChan f chan) = Chan.writeList2Chan chan . map f
    {-# INLINE writeList2Chan #-}
