{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Protocol.Connector
  ( Connector()
  , send
  , listen1
  , listen2
  , (///)
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

  -- | Perform two actions in parallel, blocking subsequent actions
  -- until both actions finish.
  -- Note: The behavior is different from `liftA2`.
  Parallel
    :: (a -> b -> c)
    -> Connector p s a
    -> Connector p s b
    -> Connector p s c

  -- | Modify the inner register state.
  RegState :: (s -> (a, s)) -> Connector p s a
  
  -- | Send a value with type `t` (which must be derived from the register `s`
  -- and must not change before sent), to port `pt`, channel `ch`.
  Send
    :: Elem2 pt ch p (Channel t)
    => Proxy pt
    -> Proxy ch
    -> (s -> t)
    -> Connector p s ()

  -- | Listen to multiple channels. After one or more succeed,
  -- the rest are blocked.
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
    -> (t -> s)
    -> Listener p s
  Listen2
    :: Listener p s
    -> Listener p s
    -> Listener p s



-- * Exposed functions
-- The exposed functions mainly use Lens APIs, and specify
-- channels with type-level parameters.


-- | Send a value to a channel.
send
  :: forall
     (pt :: Symbol) (ch :: Symbol)
     {t :: Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Elem2 pt ch p (Channel t) )
  => Getter s t
  -> Connector p s ()
send l = Send (Proxy :: Proxy pt) (Proxy :: Proxy ch) (view l)

-- | Listen to a single channel.
listen1
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     {t :: Type} {a :: Type}
     {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Elem2 pt1 ch1 p (CoChannel t) )
  => (t -> a)
  -> Setter' s a
  -> Connector p s ()
listen1 f1 l1 = Listen $
  Listen1 (Proxy :: Proxy pt1) (Proxy :: Proxy ch1) (\t -> (l1 .~ f1 t) mempty)

-- | Listen to 2 channels.
-- `listenN` for arbitrary `N` can be probably implemented with TH.
-- But for now, these are enough.
listen2
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     (pt2 :: Symbol) (ch2 :: Symbol)
     {t1 :: Type} {a1 :: Type}
     {t2 :: Type} {a2 :: Type}
     {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Elem2 pt1 ch1 p (CoChannel t1)
     , Elem2 pt2 ch2 p (CoChannel t2) )
  => (t1 -> a1)
  -> Setter' s a1
  -> (t2 -> a2)
  -> Setter' s a2
  -> Connector p s ()
listen2 f1 l1 f2 l2 = Listen $ Listen2
  (Listen1 (Proxy :: Proxy pt1) (Proxy :: Proxy ch1) (\t -> (l1 .~ f1 t) mempty))
  (Listen1 (Proxy :: Proxy pt2) (Proxy :: Proxy ch2) (\t -> (l2 .~ f2 t) mempty))


-- | Alias for `Parallel` when the result are discarded.
(///)
  :: forall
     {a :: Type} {b :: Type}
     {p :: [Type]} {s :: Type}
  .  Monoid s
  => Connector p s a
  -> Connector p s b
  -> Connector p s ()
(///) = Parallel (const (const ()))
