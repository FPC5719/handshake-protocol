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
import Control.Monad.State.Strict
import Data.Profunctor

-- | Type with an initial value. Used for implicit states.
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
      :: Maybe (i, r, s, Maybe u)
      -> (o, r, Either s (Maybe v))
    }

-- | Smart constructor handling when the @`FSM`@ is not running.
--
-- Ensure that for all @`FSM` f@, @f `Nothing` == (`mempty`,
-- `mempty`, `Right` `Nothing`)@.
fsm
  :: IsFSM r i o s
  => ((i, r, s, Maybe u) -> (o, r, Either s (Maybe v)))
  -> FSM r i o s u v
fsm f = FSM $ \case
  Nothing -> (mempty, mempty, Right Nothing)
  Just x -> f x

-- | @`Profunctor`@ instance for @u@ @v@. __Note__:
-- For @i@ @o@ it is also a Profunctor, but less
-- frequently used.
instance IsFSM r i o s => Profunctor (FSM r i o s) where
  dimap fl fr (FSM f) = fsm $ \(i, r, s, mu) ->
    (fmap . fmap . fmap $ fr) . f $ Just (i, r, s, fl <$> mu)

-- | Constraint type for @`FSM`@.
type IsFSM :: Type -> Type -> Type -> Type -> Constraint
type IsFSM r i o s =
  ( NFDataX r, Monoid r
  , Monoid o
  , NFDataX s, Initial s
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
mealyFSM' (FSM' (FSM f)) = mealy go (mempty, initial)
  where
    go (r, s) i =
      let (o, r', esv) = f $ Just (i, r, s, Just ())
          s' = either id (const initial) esv
      in ((r', s'), o)

-- | Embed a state transition into a @`FSM`@, which takes
-- one cycle to perform.
embed
  :: IsFSM r i o ()
  => ((i, Maybe u) -> r -> ((o, Maybe v), r))
  -> FSM r i o () u v
  -- ^ Finishes in one cycle so that @s ~ ()@.
embed f = fsm $ \(i, r, (), mu) ->
  let ((o, mv), r') = f (i, mu) r
  in (o, r', Right mv)

-- | Embed a @`State`@ Monad.
embedS
  :: IsFSM r i o ()
  => ((i, Maybe u) -> State r (o, Maybe v))
  -> FSM r i o () u v
  -- ^ Finishes in one cycle so that @s ~ ()@.
embedS = embed . (runState .)

-- | Skip a cycle.
skip
  :: IsFSM r i o ()
  => FSM r i o () () ()
  -- ^ Finishes in one cycle so that @s ~ ()@.
skip = embedS (const $ pure (mempty, Nothing))


-- | Sequentially combine two @`FSM`@s.
(&>)
  :: ( IsFSM r i o s
     , IsFSM r i o t
     )
  => FSM r i o s u v -- ^ Performs first.
  -> FSM r i o t v w -- ^ Performs afterwards.
  -> FSM r i o (Either s t) u w
FSM f &> FSM g = fsm $ \(i, r, est, mu) ->
  let (fo, fr, fesv) = f $ case est of
        Left s -> Just (i, r, s, mu)
        Right _ -> Nothing
      mv = case fesv of
        Left _ -> Nothing
        Right x -> x
      (go, gr, getw) = g $ case est of
        Left _ -> Nothing
        Right t -> Just (i, fr, t, mv)
  in (fo `mappend` go, gr,) $ case est of
    Left  _ -> case fesv of
      Left s' -> Left (Left s')
      Right _ -> Left (Right initial)
    Right _ -> case getw of
      Left t' -> Left (Right t')
      Right mw -> Right mw

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
FSM f &| FSM g = fsm $ \(i, r, (ms1, ms2), mu12) ->
  let (fo, fr, fesv) = f $ case ms1 of
        Nothing -> Nothing
        Just s1 -> Just (i, r, s1, fst <$> mu12)
      (go, gr, gesv) = g $ case ms2 of
        Nothing -> Nothing
        Just s2 -> Just (i, r, s2, snd <$> mu12)
  in (fo `mappend` go, fr `mappend` gr,) $ case (fesv, gesv) of
    (Left s1', Left s2') -> Left (Just s1', Just s2')
    (Left s1', Right _ ) -> Left (Just s1', Nothing )
    (Right _ , Left s2') -> Left (Nothing , Just s2')
    (Right _ , Right _ ) -> Right (Just ())

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
  let (fo, fr, fesv) = f $ Just (i, r, s1, fst <$> mu12)
      (go, gr, gesv) = g $ Just (i, r, s2, snd <$> mu12)
  in (fo `mappend` go, fr `mappend` gr,) $ case (fesv, gesv) of
    (Left s1', Left s2') -> Left (s1', s2')
    _ -> Right (Just ())

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
        (Left (), Just (Left u1))  -> Just (i, r, initial, Just u1)
        (Right (Left s1), _)       -> Just (i, r, s1, Nothing)
        _                          -> Nothing
      (go, gr, gesv) = g $ case (ees, meu) of
        (Left (), Just (Right u2)) -> Just (i, r, initial, Just u2)
        (Right (Right s2), _)      -> Just (i, r, s2, Nothing)
        _                          -> Nothing
      nst = case (ees, meu) of
        (Left (), Just (Left _))  -> Just (Left  ())
        (Right (Left _), _)       -> Just (Left  ())
        (Left (), Just (Right _)) -> Just (Right ())
        (Right (Right _), _)      -> Just (Right ())
        (Left (), Nothing)        -> Nothing
  in case nst of
    Nothing -> (mempty, mempty, Left (Left ()))
    Just (Left  ()) -> (fo, fr,) $ case fesv of
      Left s' -> Left (Right (Left s'))
      Right v -> Right v
    Just (Right ()) -> (go, gr,) $ case gesv of
      Left s' -> Left (Right (Right s'))
      Right v -> Right v

-- | Loop until the predicate is satisfied.
-- The @`FSM`@ would be invoked for at least once.
loop
  :: IsFSM r i o s
  => (Maybe u -> Bool) -- ^ Predicate.
  -> FSM r i o s u u
  -> FSM r i o s u u
loop p (FSM f) = fsm $ \(i, r, s, mu) ->
  let (fo, fr, fesu) = f $ Just (i, r, s, mu)
  in (fo, fr,) $ case fesu of
    Left s' -> Left s'
    Right mu' -> if p mu'
      then Right mu'
      else Left initial
