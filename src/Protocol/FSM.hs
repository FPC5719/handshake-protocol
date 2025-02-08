{-# LANGUAGE FlexibleInstances #-}

module Protocol.FSM where

import Clash.Prelude


class Initial a where
  initial :: a

instance (Initial a, Initial b) => Initial (Either a b) where
  initial = Left initial

instance (Initial a, Initial b) => Initial (a, b) where
  initial = (initial, initial)

instance Initial a => Initial (Maybe a) where
  initial = Just initial


data FSM s i o = FSM
  { unFSM :: Maybe i -> s -> (o, Maybe s) }

type IsFSM s i o = (Initial s, NFDataX s, Monoid o)

fsm
  :: IsFSM s i o
  => (i -> s -> (o, Maybe s))
  -> FSM s i o
fsm f = FSM $ \case
  Nothing -> const (mempty, Nothing)
  Just i  -> f i

idFSM
  :: IsFSM s i o
  => FSM s i o
idFSM = fsm $ const (\s -> (mempty, Just s))

(&>)
  :: (IsFSM s i o, IsFSM t i o)
  => FSM s i o
  -> FSM t i o
  -> FSM (Either s t) i o
FSM f &> FSM g = fsm $ \i est ->
  let (fo, fso) = uncurry f (either (Just i,) (const (Nothing, initial)) est)
      (go, gso) = uncurry g (either (const (Nothing, initial)) (Just i,) est)
  in (fo `mappend` go,) $ case est of
    Left  _ -> maybe (Just . Right $ initial) (Just . Left ) fso
    Right _ -> maybe Nothing                  (Just . Right) gso

(&|)
  :: (IsFSM s i o, IsFSM t i o)
  => FSM s i o
  -> FSM t i o
  -> FSM (Maybe s, Maybe t) i o
FSM f &| FSM g = fsm $ \i (ms, mt) ->
  let (fo, fso) = uncurry f (maybe (Nothing, initial) (Just i,) ms)
      (go, gso) = uncurry g (maybe (Nothing, initial) (Just i,) mt)
  in (fo `mappend` go,) $ case (fso, gso) of
    (Nothing, Nothing) -> Nothing
    st -> Just st

(&!)
  :: (IsFSM s i o, IsFSM t i o)
  => FSM s i o
  -> FSM t i o
  -> FSM (Maybe s, Maybe t) i o
FSM f &! FSM g = fsm $ \i (ms, mt) ->
  let (fo, fso) = uncurry f (maybe (Nothing, initial) (Just i,) ms)
      (go, gso) = uncurry g (maybe (Nothing, initial) (Just i,) mt)
  in (fo `mappend` go,) $ case (fso, gso) of
    (Just s, Just t) -> Just (Just s, Just t)
    _ -> Nothing

withRes
  :: (IsFSM s (r, i) (r, o), IsFSM (r, s) i o)
  => FSM s (r, i) (r, o)
  -> FSM (r, s) i o
withRes (FSM f) = fsm $ \i (r, s) ->
  let ((fro, fo), fso) = f (Just (r, i)) s
  in (fo, (fro,) <$> fso)

cond
  :: IsFSM s i o
  => (i -> Bool)
  -> FSM s i o
  -> FSM (Maybe s) i o
cond p (FSM f) = fsm $ \i ms ->
  let (fo, fso) = uncurry f (maybe (if p i then Just i else Nothing, initial) (Just i,) ms)
  in (fo,) $ case fso of
    Nothing -> Nothing
    Just s  -> Just (Just s)
