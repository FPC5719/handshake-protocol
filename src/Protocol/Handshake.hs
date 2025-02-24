module Protocol.Handshake where

import Protocol.FSM

import Clash.Prelude
import Control.Lens



data Ready = Ready
  { isReady :: Bool }
  deriving (Generic, NFDataX, Lift, Eq)

instance Semigroup Ready where
  Ready a <> Ready b = Ready (a || b)

instance Monoid Ready where
  mempty = Ready False
  mappend = (<>)

data Channel a = Channel (Maybe a)
  deriving (Generic, NFDataX, Lift, Eq)

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


class Validated a where
  isValid :: a -> Bool

instance Validated Ready where
  isValid = isReady

instance Validated (Channel a) where
  isValid (Valid _) = True
  isValid Invalid = False



send
  :: IsFSM r i o ()
  => Getter i Ready
  -> Setter' o (Channel a)
  -> Getter r a
  -> FSM r i o () u v
send ready out reg =
  rmap snd . loop (isReady . fst) . embedS $ \inp -> do
  x <- use reg
  pure (inp ^. ready, mempty & out .~ Valid x)

listen
  :: ( IsFSM r () i o
     , Monoid (f a)
     , Applicative f
     )
  => Getter i (Channel a)
  -> Setter' o Ready
  -> Setter' r (f a)
  -> FSM r () i o
listen value ready reg =
  rmap snd . loop (isValid . fst) . embedS $ \inp -> do
  case inp ^. value of
    Invalid -> reg .= mempty
    Valid x -> reg .= pure x
  pure (inp ^. value, mempty & ready .~ Ready True)
