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
import Data.Profunctor



-- | @`FSM` r i o s u v@: Finite State Machine.
--
-- Type parameters:
--
--     [@r@]: User defined public registers.
--     [@i@]: Input.
--     [@o@]: Output.
--     [@s@]: Auto generated state.
--     [@u@]: Data from the last @`FSM`@.
--     [@v@]: Data to the next @`FSM`@.
--
-- @`FSM`@ is indeed a wrapper around the function type
-- @i -> r -> `Either` s u -> `Either` (o, r, s) v@.
--
-- The argument @`Either` s u@ indicates whether the
-- @`FSM`@ is newly invoked with data @u@, or is already
-- working on state @s@.
--
-- The result, @`Either` (o, r, s) v@, indicates whether
-- the @`FSM`@ terminates with data @v@, or transfers to
-- a new state @(r, s)@ with output @o@.
data FSM r i o s u v
  = FSM
    { unFSM
      :: i -> r -> Either s u
      -> Either (o, r, s) v
    }


-- | @`Profunctor`@ instance for @u@ @v@. __Note__:
-- For @i@ @o@ it is also a Profunctor, but less
-- frequently used.
instance IsFSM r i o s => Profunctor (FSM r i o s) where
  dimap fl fr (FSM f) = FSM $ \i r esu ->
    fr <$> f i r (fl <$> esu)


-- | Constraint type for @`FSM`@.
type IsFSM :: Type -> Type -> Type -> Type -> Constraint
type IsFSM r i o s =
  ( NFDataX r, Monoid r
  , Monoid o
  , NFDataX s
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
        Right _ -> ((r, Nothing), mempty)

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
    -- Only finish when both are finished
    (Nothing, Nothing) -> Right ()
    -- Otherwise compose the result
    (Just (fo, fr, s1'), Nothing) -> Left (fo, fr, (Just s1', Nothing))
    (Nothing, Just (go, gr, s2')) -> Left (go, gr, (Nothing, Just s2'))
    (Just (fo, fr, s1'), Just (go, gr, s2')) ->
      Left (fo `mappend` go, fr `mappend` gr, (Just s1', Just s2'))

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
FSM f &! FSM g = FSM $ \i r esu ->
  let (fi, gi) = case esu of
        Left (s1, s2)  -> (Left s1, Left s2)
        Right (u1, u2) -> (Right u1, Right u2)
  in case (f i r fi, g i r gi) of
    (Left (fo, fr, s1'), Left (go, gr, s2')) ->
      Left (fo `mappend` go, fr `mappend` gr, (s1', s2'))
    -- Finish once either is finished
    _ -> Right ()

-- | Conditionally combine two @`FSM`@s, invoking one branch
-- based on an @`Either`@ value.
(&+)
  :: ( IsFSM r i o s1
     , IsFSM r i o s2
     )
  => FSM r i o s1 u1 v -- ^ Branch on @`Left`@.
  -> FSM r i o s2 u2 v -- ^ Branch on @`Right`@.
  -> FSM r i o (Either s1 s2) (Either u1 u2) v
FSM f &+ FSM g = FSM $ \i r esu ->
  let inp = case esu of
        -- Determine which branch to choose
        Left  (Left s1)  -> Left  (Left  s1)
        Right (Left u1)  -> Left  (Right u1)
        Left  (Right s2) -> Right (Left  s2)
        Right (Right u2) -> Right (Right u2)
  in case inp of
    Left fi -> case f i r fi of
      Left (o, r', s1') -> Left (o, r', Left s1')
      Right v -> Right v
    Right gi -> case g i r gi of
      Left (o, r', s2') -> Left (o, r', Right s2')
      Right v -> Right v

-- | Loop until the predicate is satisfied.
-- The @`FSM`@ would be invoked for at least once.
loop
  :: IsFSM r i o s
  => (u -> Bool) -- ^ Predicate.
  -> FSM r i o s () u
  -> FSM r i o (Maybe s) () u -- ^ Add one more state.
loop p (FSM f) = FSM $ \i r ems ->
  let fi = case ems of
        -- Restart from previous loop
        Left Nothing  -> Right ()
        -- Continue running
        Left (Just s) -> Left s
        -- New start from last @FSM@
        Right ()      -> Right ()
  in case f i r fi of
    Left (o, r', s') -> Left (o, r', Just s')
    Right u -> if p u
      then Right u -- Predicate is satisfied
      else Left (mempty, r, Nothing) -- Restart
