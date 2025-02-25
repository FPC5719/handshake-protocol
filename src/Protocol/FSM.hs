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

{-# LANGUAGE StandaloneKindSignatures #-}

module Protocol.FSM where

import Clash.Prelude
import Data.Either
import Data.Profunctor
{-
-- | Type with an initial value. Used for implicit states.
class Initial a where
  initial :: a

instance Initial () where
  initial = ()

instance Initial Bool where
  initial = False

instance (Initial a, Initial b) => Initial (Either a b) where
  initial = Left initial

instance (Initial a, Initial b) => Initial (a, b) where
  initial = (initial, initial)

instance Initial a => Initial (Maybe a) where
  initial = Just initial
-}
-- | @`FSM` r i o s u v@: Finite State Machine.
--
-- Type parameters:
--
--     [@r@]: User defined public registers.
--     [@i@]: Input.
--     [@o@]: Output.
--     [@s@]: Auto generated state.
--     [@u@]: Data from the last FSM.
--     [@v@]: Data to the next FSM.
--
-- @`FSM`@ is indeed a wrapper around the function type
-- @`Maybe` (i, r, s, `Maybe` u) -> (o, r, `Either` s
-- (`Maybe` v))@. The input @`Maybe`@ determines whether
-- the @`FSM`@ is running or not. @`Either`@ in the output
-- controls state transition, in which @`Left` s@ means
-- updating the state of the current @`FSM`@, and
-- @`Right` v@ means terminating the current @`FSM`@,
-- sending data @v@ to the next @`FSM`@.
data FSM r i o s u v
  = FSM
    { unFSM
      :: i -> r -> Either s u
      -> Either (o, r, s) v
    }

{-
-- | @`Profunctor`@ instance for @u@ @v@. __Note__:
-- For @i@ @o@ it is also a Profunctor, but less
-- frequently used.
instance IsFSM r i o s => Profunctor (FSM r i o s) where
  dimap fl fr (FSM f) = fsm $ \(i, r, s, mu) ->
    (fmap . fmap $ fr) . f $ Just (i, r, s, fl <$> mu)
-}

-- | Constraint type for @`FSM`@.
type IsFSM :: Type -> Type -> Type -> Type -> Constraint
type IsFSM r i o s =
  ( NFDataX r, Monoid r
  , Monoid o
  , NFDataX s -- , Initial s
  )

-- | Wrap @`FSM`@ with existential type so that @s@ can be
-- automatically determined and omitted. Meanwhile restrict
-- the type of @u@ and @v@ as @()@.
data FSM' r i o
  = forall s
  . IsFSM r i o s => FSM' (FSM r i o s () ())

-- | Convert a @`FSM'`@ into a Mealy machine.
mealyFSM'
  :: HiddenClockResetEnable dom
  => FSM' r i o
  -> (Signal dom i -> Signal dom o)
mealyFSM' (FSM' (FSM f)) = mealy go (mempty, Nothing)
  where
    go (r, ms) i =
      case f i r $ maybe (Right ()) Left ms of
        Left (o, r', s') -> ((r', Just s'), o)
        Right _ -> ((mempty, Nothing), mempty)

-- | Embed a state transition into a @`FSM`@, which takes
-- one cycle to perform.
embed
  :: IsFSM r i o ()
  => (i -> r -> u -> (o, r))
  -> (i -> r -> v)
  -> FSM r i o () u v
  -- ^ Finishes in one cycle so that @s ~ ()@.
embed f g = FSM $ \i r -> \case
  Left () -> Right (g i r)
  Right u -> let (o, r') = f i r u in Left (o, r', ())

-- | Skip a cycle.
skip
  :: IsFSM r i o ()
  => FSM r i o () () ()
  -- ^ Finishes in one cycle so that @s ~ ()@.
skip = embed (\_ r _ -> (mempty, r)) (\_ _ -> ())


-- | Sequentially combine two @`FSM`@s.
(&>)
  :: ( IsFSM r i o s
     , IsFSM r i o t
     )
  => FSM r i o s u v -- ^ Performs first.
  -> FSM r i o t v w -- ^ Performs afterwards.
  -> FSM r i o (Either s t) u w
FSM f &> FSM g = FSM $ \i r eestu ->
  let fo = f i r <$> case eestu of
        -- @f@ is running
        Left (Left  s) -> Right $ Left s
        -- @g@ is running, so block @f@
        Left (Right t) -> Left  $ t
        -- Invoke @f@
        Right u        -> Right $ Right u
      go = g i r <$> case fo of
        -- @g@ is running
        Left t                   -> Right $ Left t
        -- @f@ is running, forward its result
        Right (Left (o, r', s')) -> Left  $ (o, r', s')
        -- Invoke @g@
        Right (Right v)          -> Right $ Right v
  in case go of
    Left (o, r', s')         -> Left (o, r', Left  s')
    Right (Left (o, r', t')) -> Left (o, r', Right t')
    Right (Right w)          -> Right w

-- | Combine two @`FSM`@s in parallel (Ensures both are finished).
--
--  __Note__: See also @`(&!)`@ for comparison.
(&|)
  :: ( IsFSM r i o s1
     , IsFSM r i o s2
     )
  => FSM r i o s1 u1 ()
  -> FSM r i o s2 u2 ()
  -> FSM r i o (Maybe s1, Maybe s2) (u1, u2) ()
  -- ^  No data is sent to the next @`FSM`@ (@v ~ ()@),
  -- to avoid unintended registers.
FSM f &| FSM g = FSM $ \i r esu ->
  let (fi, gi) = case esu of
        Left (ms1, ms2) -> (Left <$> ms1, Left <$> ms2)
        Right (u1, u2)  -> (Just $ Right u1, Just $ Right u2)
      p Nothing = Nothing
      p (Just (Left (o, r', s))) = Just (o, r', s)
      p (Just (Right ())) = Nothing
  in case (p (f i r <$> fi), p (g i r <$> gi)) of
    (Nothing, Nothing) -> Right ()
    (Just (fo, fr, s1'), Nothing) -> Left (fo, fr, (Just s1', Nothing))
    (Nothing, Just (go, gr, s2')) -> Left (go, gr, (Nothing, Just s2'))
    (Just (fo, fr, s1'), Just (go, gr, s2')) ->
      Left (fo `mappend` go, fr `mappend` gr, (Just s1', Just s2'))
{-
-- | Combine two @`FSM`@s in parallel (Ensures at least one is finished).
--
--  __Note__: See also @`(&|)`@ for comparison.
(&!)
  :: ( IsFSM r i o s1
     , IsFSM r i o s2
     )
  => FSM r i o s1 u1 ()
  -> FSM r i o s2 u2 ()
  -> FSM r i o (s1, s2) (u1, u2) ()
  -- ^  No data is sent to the next @`FSM`@ (@v ~ ()@),
  -- to avoid unintended registers.
FSM f &! FSM g = fsm $ \(i, r, (s1, s2), mu12) ->
  let fe = f $ Just (i, r, s1, fst <$> mu12)
      ge = g $ Just (i, r, s2, snd <$> mu12)
  in case (fe, ge) of
    (Left (fo, fr, s'), Left (go, gr, t')) ->
      Left (fo `mappend` go, fr `mappend` gr, (s', t'))
    _ -> Right (Just ())
{-
-- | Conditionally combine two @`FSM`@s, invoking one branch
-- based on an @`Either`@ value.
(&+)
  :: ( IsFSM r i o s1
     , IsFSM r i o s2
     )
  => FSM r i o s1 u1 v
  -> FSM r i o s2 u2 v
  -> FSM r i o (Either () (Either s1 s2)) (Either u1 u2) v
  -- ^ Cannot replace @`Either` () a@ with @`Maybe` a@, because
  -- of the difference in @`Initial`@ instance.
FSM f &+ FSM g = fsm $ \(i, r, ees, meu) ->
  let (fo, fr, fesv) = f $ case (ees, meu) of
        -- Init and select Left.
        (Left (), Just (Left u1))  -> Just (i, r, initial, Just u1)
        -- Continuing with Left.
        (Right (Left s1), _)       -> Just (i, r, s1, Nothing)
        _                          -> Nothing
      (go, gr, gesv) = g $ case (ees, meu) of
        -- Init and select Right.
        (Left (), Just (Right u2)) -> Just (i, r, initial, Just u2)
        -- Continuing with Right.
        (Right (Right s2), _)      -> Just (i, r, s2, Nothing)
        _                          -> Nothing
      -- Calculate the next state.
      nst = case (ees, meu) of
        -- Leave the initial state.
        (Left (), Just (Left _))  -> Just (Left  ())
        (Right (Left _), _)       -> Just (Left  ())
        (Left (), Just (Right _)) -> Just (Right ())
        (Right (Right _), _)      -> Just (Right ())
        -- Stay at the initial state.
        (Left (), Nothing)        -> Nothing
  in case nst of
    Nothing -> (mempty, mempty, Left (Left ()))
    Just (Left  ()) -> (fo, fr,) $ case fesv of
      Left s' -> Left (Right (Left s'))
      Right v -> Right v
    Just (Right ()) -> (go, gr,) $ case gesv of
      Left s' -> Left (Right (Right s'))
      Right v -> Right v
-}
-- | Loop until the predicate is satisfied.
-- The @`FSM`@ would be invoked for at least once.
loop
  :: IsFSM r i o s
  => (Maybe u -> Bool) -- ^ Predicate.
  -> FSM r i o s () u
  -> FSM r i o s () u
loop p (FSM f) = fsm $ \(i, r, s, mx) ->
  case f $ Just (i, r, s, mx) of
    Left (o, r', s') -> Left (o, r', s')
    Right mu -> if p mu
      then Right mu
      else Left (mempty, mempty, initial)
-}
