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

import Protocol.Internal.Util
import qualified Protocol.Connector as C
import Protocol.Channel

import Control.Monad.State.Strict
import Data.Proxy


-- | Existential wrapper for `Connector`.
data WrapConnector p s a
  =  forall i
  .  C.StateIndex i
  => WrapConnector (C.Connector p s i a)

-- | Existential wrapper for `State`.
data WrapState p s a
  =  forall i
  .  C.StateIndex i
  => WrapState (PortIn p -> State (i, s) (a, PortOut p))

-- TODO
runConnector
  :: C.StateIndex i
  => C.Connector p s i a
  -> (PortIn p -> State (i, s) (a, PortOut p))
runConnector = \case
  C.Pure x -> undefined
  C.Bind x f -> undefined
  C.Cond b pa pb -> undefined
  C.LiftP2 f pa pb -> undefined
  C.Infloop x -> undefined
  C.RegState f -> undefined
  C.Send (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> undefined
  C.Listen l -> case l of
    C.Listen1 (Proxy :: Proxy pt) (Proxy :: Proxy ch) f -> undefined
    C.Listen2 l1 l2 -> undefined

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
  -> (Signal dom (PortIn p) -> Signal dom (PortOut p))
mealyWrap (WrapState st) =
  fmap snd . mealyS st (C.idxInit def, mempty)


-- $type-level-conversion
-- `Channel a` is converted to `Bool -> .. -> Maybe a`, while
-- `CoChannel a` is converted to `Maybe a -> .. -> Bool`.

type PortIn p = HList (GetPortIn2 p)

class HasPortIn ts where
  type GetPortIn ts :: [Type]
instance HasPortIn '[] where
  type GetPortIn '[] = '[]
instance HasPortIn ts =>
  HasPortIn ((Channel a `Tagged` s) ': ts) where
  type GetPortIn ((Channel a `Tagged` s) ': ts) =
    (Bool `Tagged` s) ': GetPortIn ts
instance HasPortIn ts =>
  HasPortIn ((CoChannel a `Tagged` s) ': ts) where
  type GetPortIn ((CoChannel a `Tagged` s) ': ts) =
    (Maybe a `Tagged` s) ': GetPortIn ts

class HasPortIn2 tss where
  type GetPortIn2 tss :: [Type]
instance HasPortIn2 '[] where
  type GetPortIn2 '[] = '[]
instance (HasPortIn2 tss, HasPortIn ts) =>
  HasPortIn2 ((ts `Tagged2` s) ': tss) where
  type GetPortIn2 ((ts `Tagged2` s) ': tss) =
    HList (GetPortIn ts) ': GetPortIn2 tss

type PortOut p = HList (GetPortOut2 p)

class HasPortOut ts where
  type GetPortOut ts :: [Type]
instance HasPortOut '[] where
  type GetPortOut '[] = '[]
instance HasPortOut ts =>
  HasPortOut ((Channel a `Tagged` s) ': ts) where
  type GetPortOut ((Channel a `Tagged` s) ': ts) =
    (Maybe a `Tagged` s) ': GetPortOut ts
instance HasPortOut ts =>
  HasPortOut ((CoChannel a `Tagged` s) ': ts) where
  type GetPortOut ((CoChannel a `Tagged` s) ': ts) =
    (Bool `Tagged` s) ': GetPortOut ts

class HasPortOut2 tss where
  type GetPortOut2 tss :: [Type]
instance HasPortOut2 '[] where
  type GetPortOut2 '[] = '[]
instance (HasPortOut2 tss, HasPortOut ts) =>
  HasPortOut2 ((ts `Tagged2` s) ': tss) where
  type GetPortOut2 ((ts `Tagged2` s) ': tss) =
    HList (GetPortOut ts) ': GetPortOut2 tss
