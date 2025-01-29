{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module      : Protocol.Connector
Description : Monadic Connector for Handshake-Based Protocols.

This module mainly defines the `Connector` monad, which provides
basic operations for constructing an automaton that interacts
with Handshake-Based data buses.
-}

module Protocol.Connector
  -- * The Connector Monad
  ( Connector()
  -- * Exposed Functions
  -- $exposed
  , send
  , listen1
  , listen2
  , listen4
  , parallel
  , infloop
  ) where

import Clash.Prelude

import Protocol.Internal.Util
import Protocol.Channel

import Control.Lens
import Control.Monad.State.Class
import Data.Proxy

-- | The Connector Monad.
data Connector (p :: [Type]) (s :: Type) (a :: Type) where
  -- | Pure action.
  Pure :: a -> Connector p s a

  -- | Binding two actions, blocking the latter until the former finishes.
  Bind :: Connector p s a -> (a -> Connector p s b) -> Connector p s b

  -- | Perform an action forever
  Forever :: Connector p s () -> Connector p s ()

  -- | Perform two actions in parallel, blocking subsequent actions
  -- until both actions finish.
  -- @Note@: The behavior is different from `liftA2`.
  Parallel
    :: (a -> b -> c)
    -> Connector p s a
    -> Connector p s b
    -> Connector p s c

  -- | Modify the inner register state.
  RegState :: (s -> (a, s)) -> Connector p s a
  
  -- | Send a value with type `t`, to port `pt`, channel `ch`.
  Send
    :: Elem2 pt ch p (Channel t)
    => Proxy pt
    -> Proxy ch
    -> (s -> t)
    -> Connector p s ()

  -- | Listen to multiple channels.
  Listen
    :: Listener p s
    -> Connector p s ()

instance Functor (Connector p s) where
  fmap f x = Bind x (Pure . f)

instance Applicative (Connector p s) where
  pure = Pure
  liftA2 f ma mb = Bind ma (\a -> Bind mb (\b -> Pure (f a b)))

instance Monad (Connector p s) where
  (>>=) = Bind

instance MonadState s (Connector p s) where
  state = RegState


-- | Helper datatype for specifying channels to listen
data Listener (p :: [Type]) (s :: Type) where
  Listen1
    :: Elem2 pt ch p (CoChannel t)
    => Proxy pt
    -> Proxy ch
    -> (Maybe t -> (s -> s))
    -> Listener p s
  Listen2
    :: Listener p s
    -> Listener p s
    -> Listener p s



-- $exposed
--
-- The exposed functions mainly use Lens APIs, and specify
-- channels with type-level parameters.
--
-- As for `Connector p s`, the constraint `Monoid s` should
-- be always satisfied.
--
-- In practice, `s` is usually a record type where all its
-- fields are `Monoid`s like `First` or `Last`, since the
-- `Semigroup` and `Monoid` instances can be derived, and
-- the `Lens`es can be meanwhile built automatically.


-- | Send a value to a channel.
--
-- Sending to multiple channels can be expressed by
-- multiple `send`s in parallel.
send
  :: forall
     (pt :: Symbol) (ch :: Symbol)
     {t :: Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Elem2 pt ch p (Channel t)
     )
  => Getter s t
     -- ^ The data being sent must be derived from the register,
     -- and must NOT change before sent.
  -> Connector p s ()
send l = Send (Proxy @pt) (Proxy @ch) (view l)

-- | Listen to a single channel.
listen1
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     {t :: Type} {a :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a)
     , Applicative f
     , Elem2 pt1 ch1 p (CoChannel t)
     )
  => (t -> a)
     -- ^ Transform the raw data from the channel to the
     -- corresponding representation in the register.
  -> Setter' s (f a)
     -- ^ The setter of the corresponding register.
     -- In practice, each listened channel probably corresponds to
     -- separate parts of the register, while shared register parts
     -- are permitted for efficiency.
  -> Connector p s ()
listen1 f1 l1 = Listen $
  Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (pure . f1) t)

-- | Listen to 2 channels.
--
-- The channels are blocked after one or more channels' transfers
-- occur. If a transfer occur, the data is wrapped in `pure` before
-- being set in the register. Otherwise `mempty` is used instead.
-- Therefore, the subsequential program can determine whether a
-- transfer has occured or not.
listen2
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     (pt2 :: Symbol) (ch2 :: Symbol)
     {t1 :: Type} {a1 :: Type}
     {t2 :: Type} {a2 :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a1)
     , Monoid (f a2)
     , Applicative f
     , Elem2 pt1 ch1 p (CoChannel t1)
     , Elem2 pt2 ch2 p (CoChannel t2)
     )
  => (t1 -> a1)       -- ^ Transform the result from channel 1.
  -> Setter' s (f a1) -- ^ Setter 1.
  -> (t2 -> a2)       -- ^ Transform the result from channel 2.
  -> Setter' s (f a2) -- ^ Setter 2.
  -> Connector p s ()
listen2 f1 l1 f2 l2 = Listen $ Listen2
  (Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (pure . f1) t))
  (Listen1 (Proxy @pt2) (Proxy @ch2) (\t -> l2 .~ maybe mempty (pure . f2) t))

-- | Listen to 4 channels.
--
-- `listenN` for arbitrary `N` can be probably implemented with TH.
-- However, for now these are enough.
listen4
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     (pt2 :: Symbol) (ch2 :: Symbol)
     (pt3 :: Symbol) (ch3 :: Symbol)
     (pt4 :: Symbol) (ch4 :: Symbol)
     {t1 :: Type} {a1 :: Type}
     {t2 :: Type} {a2 :: Type}
     {t3 :: Type} {a3 :: Type}
     {t4 :: Type} {a4 :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a1)
     , Monoid (f a2)
     , Monoid (f a3)
     , Monoid (f a4)
     , Applicative f
     , Elem2 pt1 ch1 p (CoChannel t1)
     , Elem2 pt2 ch2 p (CoChannel t2)
     , Elem2 pt3 ch3 p (CoChannel t3)
     , Elem2 pt4 ch4 p (CoChannel t4)
     )
  => (t1 -> a1)       -- ^ Transform the result from channel 1.
  -> Setter' s (f a1) -- ^ Setter 1.
  -> (t2 -> a2)       -- ^ Transform the result from channel 2.
  -> Setter' s (f a2) -- ^ Setter 2.
  -> (t3 -> a3)       -- ^ Transform the result from channel 3.
  -> Setter' s (f a3) -- ^ Setter 3.
  -> (t4 -> a4)       -- ^ Transform the result from channel 4.
  -> Setter' s (f a4) -- ^ Setter 4.
  -> Connector p s ()
listen4 f1 l1 f2 l2 f3 l3 f4 l4 = Listen $ Listen2
  (Listen2
    (Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (pure . f1) t))
    (Listen1 (Proxy @pt2) (Proxy @ch2) (\t -> l2 .~ maybe mempty (pure . f2) t))
  )
  (Listen2
    (Listen1 (Proxy @pt3) (Proxy @ch3) (\t -> l3 .~ maybe mempty (pure . f3) t))
    (Listen1 (Proxy @pt4) (Proxy @ch4) (\t -> l4 .~ maybe mempty (pure . f4) t))
  )

-- | Alias for `Parallel` when the results are discarded.
parallel
  :: forall
     {a :: Type} {b :: Type}
     {p :: [Type]} {s :: Type}
  .  Monoid s
  => Connector p s a -- ^ The result is discarded.
  -> Connector p s b -- ^ The result is discarded.
  -> Connector p s ()
parallel = Parallel (const (const ())) 

-- | Perform an action infinitely.
infloop
  :: forall
     {p :: [Type]} {s :: Type}
  .  Monoid s
  => Connector p s () -- ^ Action for an iteration.
  -> Connector p s ()
infloop = Forever
