module Protocol.Connector where

import Clash.Prelude

import Protocol.Internal.Util

-- | The Connector Free Monad
data Connector (p :: [Type]) (s :: Type) (a :: Type) where
  Pure :: a -> Connector p s a
  Bind :: Connector p s a -> (a -> Connector p s b) -> Connector p s b

instance Functor (Connector p s) where
  fmap f x = Bind x (Pure . f)

instance Applicative (Connector p s) where
  pure = Pure
  liftA2 f ma mb = Bind ma (\a -> Bind mb (\b -> Pure (f a b)))

instance Monad (Connector p s) where
  (>>=) = Bind
