{-# LANGUAGE FlexibleInstances #-}

module Protocol.FSM where

import Clash.Prelude
import Control.Monad.State.Strict
import Data.Profunctor

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

-- | Finite State Machine
data FSM
  r -- ^ User defined state
  s -- ^ Auto generated state
  i -- ^ Input
  o -- ^ Output
  = FSM
    { unFSM
      :: (Maybe i, r, s) -- ^ `Maybe i`: `Nothing` if not executing.
      -> (o, r, Maybe s) -- ^ `Maybe s`: `Nothing` if terminate.
    }

instance Profunctor (FSM r s) where
  dimap l r (FSM f) = FSM $ \(mi, ri, s) ->
    let (o, ro, ms) = f ((l <$> mi), ri, s)
    in (r o, ro, ms)

type IsFSM r s i o = (NFDataX r, Monoid r, Initial s, NFDataX s, Monoid o)

data FSM' r i o = forall s . IsFSM r s i o => FSM' (FSM r s i o)

-- | Smart constructor handling when `Maybe i` is `Nothing`.
fsm
  :: IsFSM r s i o
  => ((i, r, s) -> (o, r, Maybe s))
  -> FSM r s i o
fsm f = FSM $ \(mi, r, s) -> case mi of
  Nothing -> (mempty, r, Nothing)
  Just i  -> f (i, r, s)

-- | Embed a state transition into a FSM, taking one cycle to perform.
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

skip
  :: IsFSM r () i o
  => FSM r () i o
skip = embedS (const $ pure mempty)

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
