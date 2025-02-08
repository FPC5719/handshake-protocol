{-# LANGUAGE FlexibleInstances #-}

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

-- import Protocol.Internal.Util
import qualified Protocol.Connector as C
import Protocol.Index
import Protocol.Interface
-- import Protocol.Channel

import Control.Monad.State.Strict
import Data.Proxy


-- | Existential wrapper for `Connector`.
data WrapConnector p s a
  =  forall i
  .  WrapConnector (C.Connector p s i a)

-- | Existential wrapper for `State`.
data WrapState p s a
  =  forall i
  .  WrapState (HList (FInput p) -> State (i, s) (a, HList (FOutput p)))

-- TODO
runConnector
  :: ( Monoid (HList (FOutput p))
     )
  => C.Connector p s i a
  -> (HList (FInput p) -> State (i, s) (a, HList (FOutput p)))
runConnector = \case
  C.Pure x -> \_ -> pure (x, mempty)
  C.Bind x f -> \inp ->
    let ((), (xa, o1)) = runState (runConnector x inp)
        
    in
  C.Cond b x y -> undefined
  C.Until mx -> undefined

{-
runConnectorWrap
  :: WrapConnector p s a
  -> WrapState p s a
runConnectorWrap (WrapConnector con) =
  WrapState (runConnector con)

-- | Convert a `State` monad to a Mealy Machine.
mealyWrap
  :: ( HiddenClockResetEnable dom
     , NFDataX s
     , Monoid s
     )
  => WrapState p s ()
  -> (Signal dom (HList (FInput p)) -> Signal dom (HList (FOutput p)))
mealyWrap (WrapState st) =
  fmap snd . mealyS st (def, mempty)
-}
