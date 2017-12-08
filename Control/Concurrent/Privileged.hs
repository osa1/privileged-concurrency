-- | Module: Control.Concurrent.Privileged
-- Copyright: (c) Jeff Shaw 2012
-- License: BSD, see the file LICENSE
-- Maintainer: omeragacan@gmail.com
-- Stability: experimental
-- Portability: non-portable (concurrency)
--
-- Privilege separated concurrency abstractions.

module Control.Concurrent.Privileged (
-- * Privilege Separated Concurrent Haskell
module Control.Concurrent.Chan.Class,
module Control.Concurrent.Chan.ReadOnly,
module Control.Concurrent.Chan.WriteOnly,
module Control.Concurrent.MVar.Class,
module Control.Concurrent.MVar.ReadOnly,
module Control.Concurrent.MVar.WriteOnly,
module Control.Concurrent.STM.TVar.ReadOnly,
module Control.Concurrent.STM.TVar.WriteOnly,
module Control.Concurrent.STM.TMVar.ReadOnly,
module Control.Concurrent.STM.TMVar.WriteOnly,
module Control.Concurrent.STM.TChan.ReadOnly,
module Control.Concurrent.STM.TChan.WriteOnly,
-- * Re-exports
module Control.Monad.IO.Class,
Chan.Chan,
Chan.newChan,
MVar.modifyMVar,
MVar.modifyMVar_,
MVar.MVar,
MVar.newEmptyMVar,
MVar.newMVar,
TChan.newTChan,
TChan.TChan,
TMVar.newEmptyTMVar,
TMVar.newTMVar,
TMVar.TMVar,
TVar.newTVar,
TVar.TVar
) where

import Control.Concurrent.Chan.Class
import Control.Concurrent.Chan.ReadOnly
import Control.Concurrent.Chan.WriteOnly
import Control.Concurrent.MVar.Class
import Control.Concurrent.MVar.ReadOnly
import Control.Concurrent.MVar.WriteOnly
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TChan.ReadOnly
import Control.Concurrent.STM.TChan.WriteOnly
import qualified Control.Concurrent.STM.TMVar as TMVar
import Control.Concurrent.STM.TMVar.ReadOnly
import Control.Concurrent.STM.TMVar.WriteOnly
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar.ReadOnly
import Control.Concurrent.STM.TVar.WriteOnly
import Control.Monad.IO.Class
import qualified UnliftIO.Chan as Chan
import qualified UnliftIO.MVar as MVar

{- $intro

GHC's containers for communication between threads, such as MVar,
allow any thread with access to a mutable unit to perform any
operation on that unit. However, there are times when there needs to
be guarantees about what can read or write to a particular
location. For example, a thread which is intended to only produce
values for a Chan should not be able to consume values from the same
Chan. If instead the function were to receive a WriteOnlyChan, then the
user of the producer function could rest assured that it would never
consume values.

Another possible scenario is that you are writing a library that has
some complicated interaction between threads, and exposing a bare Chan
to the user of your library could break some invariants that you want
to guarantee. Rather than providing a bare Chan, you can provide a
ReadOnlyChan or WriteOnlyChan, therefore protecting your invariants.

-}
