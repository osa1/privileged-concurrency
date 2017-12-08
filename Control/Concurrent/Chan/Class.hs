module Control.Concurrent.Chan.Class where

import qualified UnliftIO.Chan as Chan
import Control.Monad.IO.Unlift

class ChanDup chan where
    dupChan :: MonadIO m => chan a -> m (chan a)

instance ChanDup Chan.Chan where
    dupChan = Chan.dupChan
    {-# INLINE dupChan #-}

class ChanWrite chan where
    writeChan :: MonadIO m => chan a -> a -> m ()
    writeList2Chan :: MonadIO m => chan a -> [a] -> m ()

instance ChanWrite Chan.Chan where
    writeChan = Chan.writeChan
    {-# INLINE writeChan #-}

    writeList2Chan = Chan.writeList2Chan
    {-# INLINE writeList2Chan #-}

class ChanRead chan where
    readChan :: MonadIO m => chan a -> m a
