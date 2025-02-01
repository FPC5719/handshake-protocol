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

import Control.Monad.State.Strict
import Data.Proxy


-- TODO
data PortIn p = PortIn ()
data PortOut p = PortOut ()

-- | Existential wrapper for `State`.
data WrapState p s a
  =  forall i
  .  StateIndex i
  => WrapState (PortIn p -> State (i, s) (a, PortOut p))

-- TODO
runConnector
  :: WrapConnector p s a
  -> WrapState p s a
runConnector (WrapConnector con) = case con of
  Pure x -> undefined
  Bind x f -> undefined
  Infloop x -> undefined
  LiftP2 f pa pb -> undefined
  RegState f -> undefined
  Send (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> undefined
  Listen l -> case l of
    Listen1 (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> undefined
    Listen2 l1 l2 -> undefined


mealyWrap
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     )
  => WrapState p s ()
  -> s
  -> (Signal dom (PortIn p) -> Signal dom (PortOut p))
mealyWrap (WrapState st) initS =
  fmap snd . mealyS st (idxInit, initS)
