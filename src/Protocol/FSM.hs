{-|
Module      : Protocol.FSM
Description : Representation of Finite State Machines
License     : BSD-2-Clause
Maintainer  : fpc5719@163.com

This module provides a kind of wrapped representation of
Finite State Machines, `FSM`, along with basic combinators.

Motivations:

    * __Automatically managed state transitions.__
    Combinators handle state transitions while states are
    indicated by polymorphic type parameters.

    * __Composability and reusability.__
    This package provides basic components that interact
    with various hardware protocols.
-}

module Protocol.FSM where

import Clash.Prelude
import Control.Monad.State.Strict
import Data.Maybe
import Data.Profunctor

-- | Type with an initial value.
class Initial a where
  initial :: a

instance Initial () where
  initial = ()

instance (Initial a, Initial b) => Initial (Either a b) where
  initial = Left initial

instance (Initial a, Initial b) => Initial (a, b) where
  initial = (initial, initial)

instance Initial a => Initial (Maybe a) where
  initial = Just initial

-- | Finite State Machine.
--
-- Type parameters:
--
--     [@r@]: User defined state
--     [@s@]: Auto generated state
--     [@i@]: Input
--     [@o@]: Output
data FSM r s i o
  = FSM
    { unFSM
      :: (Maybe i, r, s)
      -> (o, r, Maybe s)
      -- ^ @`Maybe` i@: @`Nothing`@ if not executing.
      -- @`Maybe` s@: @`Nothing`@ if terminate.
    }

instance Profunctor (FSM r s) where
  dimap l r (FSM f) = FSM $ \(mi, ri, s) ->
    let (o, ro, ms) = f ((l <$> mi), ri, s)
    in (r o, ro, ms)

-- | Constraint type for @`FSM`@.
type IsFSM r s i o = (NFDataX r, Monoid r, Initial s, NFDataX s, Monoid o)

-- | Wrap @`FSM`@ with existential type so that @s@ can be automatically
-- determined and omitted.
data FSM' r i o = forall s . IsFSM r s i o => FSM' (FSM r s i o)

-- | Smart constructor handling when @Maybe i@ is @Nothing@.
fsm
  :: IsFSM r s i o
  => ((i, r, s) -> (o, r, Maybe s))
  -> FSM r s i o
fsm f = FSM $ \(mi, r, s) -> case mi of
  Nothing -> (mempty, r, Nothing)
  Just i  -> f (i, r, s)

-- | Convert a @`FSM`@ into a Mealy machine.
mealyFSM
  :: ( IsFSM r s i o
     , HiddenClockResetEnable dom
     )
  => FSM r s i o
  -> (Signal dom i -> Signal dom o)
mealyFSM (FSM f) = mealy go (mempty, initial)
  where
    go (r, s) inp =
      let (o, r', ms) = f (Just inp, r, s)
      in ((r', fromMaybe initial ms), o)

-- | Convert a @`FSM'`@ into a Mealy machine.
mealyFSM'
  :: ( HiddenClockResetEnable dom
     )
  => FSM' r i o
  -> (Signal dom i -> Signal dom o)
mealyFSM' (FSM' f) = mealyFSM f

-- | Embed a state transition into a @`FSM`@, which takes
-- one cycle to perform.
embed
  :: IsFSM r () i o
  => (i -> r -> (o, r))
  -> FSM r () i o
embed f = fsm $ \(i, r, ()) ->
  let (o, ro) = f i r
  in (o, ro, Nothing)

-- | Embed a `State` Monad.
embedS
  :: IsFSM r () i o
  => (i -> State r o)
  -> FSM r () i o
embedS = embed . (runState .)

-- | Skip a cycle.
skip
  :: IsFSM r () i o
  => FSM r () i o
skip = embedS (const $ pure mempty)

-- | Sequentially combine two @`FSM`@s. Consider:
--
-- > f :: FSM r s i o
-- > g :: FSM r t i o
-- > h = f &> g :: FSM r (Either s t) i o
--
-- When @h@ is invoked, it first invokes @f@ and blocks @g@.
-- When the returned state of @f@ (@Maybe s@) reaches
-- @Nothing@, @f@ is blocked and @g@ is invoked. When @g@
-- finishes, @h@ finishes.
(&>)
  :: (IsFSM r s i o, IsFSM r t i o)
  => FSM r s i o
  -> FSM r t i o
  -> FSM r (Either s t) i o
FSM f &> FSM g = fsm $ \(i, r, est) ->
  let (fo, fr, fso) = f (either (Just i, r,) (const (Nothing, r, initial)) est)
      (go, gr, gso) = g (either (const (Nothing, fr, initial)) (Just i, fr,) est)
  in (fo `mappend` go, gr,) $ case est of
    Left  _ -> maybe (Just . Right $ initial) (Just . Left ) fso
    Right _ -> maybe Nothing                  (Just . Right) gso

-- | Combine two @`FSM`@s in parallel (Ensures both are finished).
-- Consider:
--
-- > f :: FSM r s i o
-- > g :: FSM r t i o
-- > h = f &| g :: FSM r (Maybe s, Maybe t) i o
--
-- When @h@ is invoked, @f@ and @g@ are invoked simultaneously.
-- When either @f@ or @g@ is finished, the finished one is blocked,
-- until the other one also finishes. When both @f@ and @g@ finished,
-- @h@ finishes.
--
--  __Note__: See also @`(&!)`@ for comparison.
(&|)
  :: (IsFSM r s i o, IsFSM r t i o)
  => FSM r s i o
  -> FSM r t i o
  -> FSM r (Maybe s, Maybe t) i o
FSM f &| FSM g = fsm $ \(i, r, (ms, mt)) ->
  let (fo, fr, fso) = f (maybe (Nothing, r, initial) (Just i, r,) ms)
      (go, gr, gso) = g (maybe (Nothing, r, initial) (Just i, r,) mt)
  in (fo `mappend` go, fr `mappend` gr,) $ case (fso, gso) of
    (Nothing, Nothing) -> Nothing
    st -> Just st

-- | Combine two @`FSM`@s in parallel (Ensures at least one is finished).
-- Consider:
--
-- > f :: FSM r s i o
-- > g :: FSM r t i o
-- > h = f &| g :: FSM r (Maybe s, Maybe t) i o
--
-- When @h@ is invoked, @f@ and @g@ are invoked simultaneously.
-- When either @f@ or @g@ is finished, or if both @f@ and @g@
-- finishes at the same time, @h@ finishes.
--
-- If one @`FSM`@ finishes before the other, the latter is
-- forced to terminate, which may or may not be the desired
-- behavior.
--
--  __Note__: See also @`(&|)`@ for comparison.
(&!)
  :: (IsFSM r s i o, IsFSM r t i o)
  => FSM r s i o
  -> FSM r t i o
  -> FSM r (Maybe s, Maybe t) i o
FSM f &! FSM g = fsm $ \(i, r, (ms, mt)) ->
  let (fo, fr, fso) = f (maybe (Nothing, r, initial) (Just i, r,) ms)
      (go, gr, gso) = g (maybe (Nothing, r, initial) (Just i, r,) mt)
  in (fo `mappend` go, fr `mappend` gr,) $ case (fso, gso) of
    (Just s, Just t) -> Just (Just s, Just t)
    _ -> Nothing

-- | Invoke an @`FSM`@ only when the predicate is satisfied.
-- Otherwise the behavior should be the same as @`skip`@,
-- i.e. the result always takes at least one cycle.
cond
  :: IsFSM r s i o
  => ((i, r) -> Bool)
  -> FSM r s i o
  -> FSM r (Maybe s) i o
cond p (FSM f) = fsm $ \(i, r, ms) ->
  let (fo, fr, fso) = f $ case ms of
        Nothing -> if p (i, r)
          then (Just i , r, initial)
          else (Nothing, r, initial)
        Just s -> (Just i, r, s)
  in (fo, fr,) $ case fso of
    Nothing -> Nothing
    Just s  -> Just (Just s)

-- | Invoke one branch based on an @Either@ value.
condCase
  :: ( IsFSM r s1 (i1, i) o
     , IsFSM r s2 (i2, i) o
     , IsFSM r (Either s1 s2) i o
     )
  => (r -> Either i1 i2)
  -> FSM r s1 (i1, i) o
  -> FSM r s2 (i2, i) o
  -> FSM r (Maybe (Either s1 s2)) i o
condCase p (FSM f1) (FSM f2) = fsm $ \(i, r, mes) ->
  let l3 (x, y, z) = (x, y, Left <$> z)
      r3 (x, y, z) = (x, y, Right <$> z)
      (fo, fr, fso) = case mes of
        Nothing -> case p r of
          Left  i1 -> l3 $ f1 (Just (i1, i), r, initial)
          Right i2 -> r3 $ f2 (Just (i2, i), r, initial)
        Just (Left s1) -> case p r of
          Left  i1 -> l3 $ f1 (Just (i1, i), r, s1)
          Right _  -> r3 $ f2 (Nothing, r, initial) -- Should not happen
        Just (Right s2) -> case p r of
          Left  _  -> l3 $ f1 (Nothing, r, initial) -- Should not happen
          Right i2 -> r3 $ f2 (Just (i2, i), r, s2)
  in (fo, fr,) $ case fso of
    Nothing -> Nothing
    Just s -> Just (Just s)

-- | Loop until the predicate is satisfied.
-- The @`FSM`@ would be invoked for at least once.
loop
  :: IsFSM r s i o
  => (o -> Bool)
  -> FSM r s i o
  -> FSM r s i o
loop p (FSM f) = fsm $ \(i, r, s) ->
  let (fo, fr, fso) = f (Just i, r, s)
  in (fo, fr,) $ case fso of
    Nothing -> if p fo
      then Nothing
      else Just initial
    st -> st
