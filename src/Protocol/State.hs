{-|
Module:      Protocol.State
Description: Convert `Connector` monad to `State` monad.

This module mainly provides conversion from a `Connector` monad
to a `State` monad, which can be later converted into a Mealy
machine.

@Note@: A `Connector` monad describes the whole life span of an
automaton, while a `State` monad represents a single clock cycle.
-}

module Protocol.State where

import Clash.Prelude

import Protocol.Internal.Util
import Protocol.Connector

import Control.Monad.State.Lazy

{-
-- | Convert a `Connector` monad to a `State` monad.
runConnector
  :: Connector p s a
  -- ^ Input `Connector` monad.
  -> (Unsigned n, Unsigned n -> State (Unsigned n, s) a)
  -- ^ Delta state index, and the result `State` monad.
runConnector con = case con of
  Pure x -> (0, \_ -> pure x)
  Bind x f -> (dltx + dltf, st)
    where
      (dltx, stx) = runConnector x
  Forever x -> ()
  Parallel f pa pb -> ()
  RegState f -> ()
  Send (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> ()
  Listen l -> case l of
    Listen1 (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> ()
    Listen2 l1 l2 -> ()
-}
