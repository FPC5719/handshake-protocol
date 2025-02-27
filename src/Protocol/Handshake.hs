module Protocol.Handshake where

import Protocol.FSM
import Protocol.Boolish

import Clash.Prelude
import Control.Lens



data Ready = Ready Bool
  deriving (Generic, NFDataX, Lift, Eq, Show)

instance Semigroup Ready where
  Ready a <> Ready b = Ready (a || b)

instance Monoid Ready where
  mempty = Ready False
  mappend = (<>)

instance Boolish Ready where
  boolify (Ready x) = x

data Channel a = Channel (Maybe a)
  deriving (Generic, NFDataX, Lift, Eq, Show)

pattern Invalid :: Channel a
pattern Invalid = Channel Nothing
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


data Sender r i o a
  = Sender
    (Getter  i Ready)
    (Setter' o (Channel a))
    (Getter  r a)

send
  :: IsFSM r i o ()
  => Sender r i o a
  -> FSM r i o () () ()
send (Sender rd ch dat) = FSM $ \i r ->
  let o = mempty & ch .~ Valid (r ^. dat)
  in \case
    Left () -> if boolify (i ^. rd)
      then Right ()
      else Left (o, r, ())
    Right () -> Left (o, r, ())


data Listener r i o a
  =  forall f
  .  (Monoid (f a), Applicative f)
  => Listener
     (Setter' o Ready)
     (Getter  i (Channel a))
     (Setter' r (f a))

listen
  :: IsFSM r i o ()
  => Listener r i o a
  -> FSM r i o () () a
listen (Listener rd ch dat) = FSM $ \i r ->
  let o = mempty & rd .~ Ready True
      r' = r & dat .~ mempty
  in \case
    Left () -> case i ^. ch of
      Invalid -> Left (o, r', ())
      Valid x -> Right x
    Right ()  -> Left (o, r', ())
