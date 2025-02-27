{-|
Module      : Protocol.Boolish
Description : Boolish typeclass
License     : BSD-2-Clause
Maintainer  : fpc5719@163.com

This module provides a typeclass, @`Boolish`@, representing
datatypes that can be turned into a @`Bool`@.
-}

module Protocol.Boolish where

import Clash.Prelude

-- | A type is @`Boolish`@, if it can be @`boolify`@ed.
class Boolish a where
  boolify :: a -> Bool

instance Boolish Bool where
  boolify = id

instance Boolish Bit where
  boolify = bitToBool

instance Boolish (Maybe a) where
  boolify Nothing  = False
  boolify (Just _) = True

instance Boolish (Either a b) where
  boolify (Left  _) = False
  boolify (Right _) = True

-- | @`Boolish`@ and.
data BAnd a b = BAnd a b
  deriving (Generic, Show, Eq, NFDataX)

instance (Boolish x, Boolish y) => Boolish (BAnd x y) where
  boolify (BAnd x y) = boolify x && boolify y

-- | @`Boolish`@ or.
data BOr a b = BOr a b
  deriving (Generic, Show, Eq, NFDataX)

instance (Boolish x, Boolish y) => Boolish (BOr x y) where
  boolify (BOr x y) = boolify x || boolify y

-- | @`boolify` (`band` x y) == `boolify` (`BAnd` x y)@.
band :: Boolish a => a -> a -> a
band x y = if boolify x then y else x

-- | @`boolify` (`bor` x y) == `boolify` (`BOr` x y)@.
bor :: Boolish a => a -> a -> a
bor x y = if boolify x then x else y
