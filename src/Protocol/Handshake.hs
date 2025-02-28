{-|
Module      : Protocol.Handshake
Description : Handshake-based Transactions
License     : BSD-2-Clause
Maintainer  : fpc5719@163.com

This module provides basic components for interacting
with handshake-based buses.
-}

module Protocol.Handshake where

import Protocol.FSM
import Protocol.Boolish

import Clash.Prelude
import Control.Lens


-- | @`Ready`@ is a wrapper over @`Bool`@.
data Ready = Ready Bool
  deriving (Generic, NFDataX, Lift, Eq, Show)

instance Semigroup Ready where
  Ready a <> Ready b = Ready (a || b)

instance Monoid Ready where
  mempty = Ready False
  mappend = (<>)

instance Boolish Ready where
  boolify (Ready x) = x

-- | @`Channel`@ either holds a valid data or
-- indicates invalid.
data Channel a = Channel (Maybe a)
  deriving (Generic, NFDataX, Lift, Eq, Show)

-- | @`Invalid`@ pattern.
pattern Invalid :: Channel a
pattern Invalid = Channel Nothing

-- | @`Valid`@ pattern.
pattern Valid :: a -> Channel a
pattern Valid a = Channel (Just a)

{-# COMPLETE Invalid, Valid #-}

instance Semigroup (Channel a) where
  Channel (Just a) <> Channel _ = Channel (Just a)
  Channel _ <> Channel b = Channel b

instance Monoid (Channel a) where
  mempty = Channel Nothing
  mappend = (<>)

instance Boolish (Channel a) where
  boolify (Channel m) = boolify m


-- | A @`Sender`@ consists 3 lenses used for sending a data.
--
-- __Note__: It should be satisfied that the data @a@ stored
-- in @r@ does not change before it is sent.
data Sender r i o a
  = Sender
    (Getter  i Ready)       -- ^ Whether the receiver is @`Ready`@.
    (Setter' o (Channel a)) -- ^ @`Channel`@ to send to.
    (Getter  r a)           -- ^ Data to send.

-- | @`send`@ a data with a @`Sender`@.
send
  :: IsFSM r i o Bool
  => Sender r i o a
  -> FSM r i o Bool () ()
send (Sender rd ch dat) = FSM $ \i r ->
  let o = mempty & ch .~ Valid (r ^. dat)
  in \case
    Left True -> Right ()
    _         -> Left (o, r, boolify (i ^. rd))


-- | A @`Listener`@ consists 3 lenses used for listening
-- for a data and store it into @r@.
--
-- __Note__: The data @a@ stored in @r@ is wrapped with
-- container @f@, because once @`listen`@ is invoked,
-- @f a@ is cleared with @`mempty`@, and a valid data is
-- stored with @`pure`@, which is convinient when listening
-- to multiple channels in parallel. Thus there are the
-- constraints @`Monoid` (f a)@ and @`Applicative` f@.
data Listener r i o a
  =  forall f
  .  (Monoid (f a), Applicative f)
  => Listener
     (Setter' o Ready)       -- ^ Whether @`Ready`@ to listen.
     (Getter  i (Channel a)) -- ^ @`Channel`@ to listen to.
     (Setter' r (f a))       -- ^ Where to store the data.

-- | @`listen`@ for a data with a @`Listener`@.
listen
  :: IsFSM r i o Bool
  => Listener r i o a
  -> FSM r i o Bool () ()
listen (Listener rd ch dat) = FSM $ \i r ->
  let o = mempty & rd .~ Ready True
  in \case
    Left True -> Right ()
    _         -> case i ^. ch of
      Invalid -> Left (o, r & dat .~ mempty, False)
      Valid x -> Left (o, r & dat .~ pure x, True)
