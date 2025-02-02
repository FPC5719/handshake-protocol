{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module      : Protocol.Connector
Description : Monadic Connector for Handshake-Based Protocols.

This module mainly defines the `Connector` monad, which provides
basic operations for constructing an automaton that interacts
with Handshake-Based data buses.
-}

module Protocol.Connector where
{-
  -- * The Connector Monad
  ( Connector(..)
  -- * Exposed Functions
  -- $exposed
  , send
  , listen1
  , listen2
  , listen4
  , parallel
  , infloop
  ) where
-}

import Clash.Prelude
  ( (.)
  , ($)
  , const
  , maybe
  , Type
  , Symbol
  , Bool(..)
  , Maybe(..)
  , Either(..)
  , Eq(..)
  , NFDataX
  , Generic
  )
import qualified Clash.Prelude as CP

import Protocol.Internal.Util
import Protocol.Channel

import Control.Lens
-- import Control.Monad.State.Class
import Data.Monoid
import Data.Proxy


-- | State index.
class (NFDataX i, Eq i, CP.Default (Outside i)) => StateIndex i where
  -- | Outside input.
  type Outside i

  -- | Initial state.
  idxInit :: Outside i -> i

  -- | `Just x` when there is a next state.
  -- `Nothing` for the last one.
  idxNext :: Outside i -> i -> Maybe i

data IndexUnit = IndexUnit
  deriving (Generic, Eq, NFDataX)

instance StateIndex IndexUnit where
  type Outside IndexUnit = ()

  idxInit () = IndexUnit

  idxNext () IndexUnit = Nothing

data IndexBind a b
  = BindA a
  | BindB b
  deriving (Generic, Eq, NFDataX)

instance (StateIndex a, StateIndex b) =>
  StateIndex (IndexBind a b) where
  type Outside (IndexBind a b) = (Outside a, Outside b)
  
  idxInit (oa, _) = BindA $ idxInit oa

  idxNext (oa, ob) = \case
    BindA idxa -> case idxNext oa idxa of
      Nothing -> Just . BindB $ idxInit ob
      Just idxa' -> Just . BindA $ idxa'
    BindB idxb -> case idxNext ob idxb of
      Nothing -> Nothing
      Just idxb' -> Just . BindB $ idxb'

data OutsideChoice a b
  = ChoiceLeft a
  | ChoiceRight b
  deriving (Generic, Eq, NFDataX)
instance CP.Default a => CP.Default (OutsideChoice a b) where
  def = ChoiceLeft CP.def

data IndexCond a b
  = CondA a
  | CondB b
  deriving (Generic, Eq, NFDataX)

instance (StateIndex a, StateIndex b) =>
  StateIndex (IndexCond a b) where
  type Outside (IndexCond a b) = OutsideChoice (Outside a) (Outside b)
  
  idxInit = \case
    ChoiceLeft oa -> CondA $ idxInit oa
    ChoiceRight ob -> CondB $ idxInit ob

  idxNext o idx = case (o, idx) of
    (ChoiceLeft oa, CondA idxa) -> case idxNext oa idxa of
      Nothing -> Nothing
      Just idxa' -> Just (CondA idxa')
    (ChoiceRight ob, CondB idxb) -> case idxNext ob idxb of
      Nothing -> Nothing
      Just idxb' -> Just (CondB idxb')
    _ -> Just idx

data IndexParallel a b
  = ParallelBoth a b
  | ParallelA a
  | ParallelB b
  deriving (Generic, Eq, NFDataX)

instance (StateIndex a, StateIndex b) =>
  StateIndex (IndexParallel a b) where
  type Outside (IndexParallel a b) = (Outside a, Outside b)

  idxInit (oa, ob) = ParallelBoth (idxInit oa) (idxInit ob)

  idxNext (oa, ob) =
    let idxStep = \case
          (Nothing, Nothing) -> Nothing
          (Nothing, Just idxb') -> Just $ ParallelB idxb'
          (Just idxa', Nothing) -> Just $ ParallelA idxa'
          (Just idxa', Just idxb') -> Just $ ParallelBoth idxa' idxb'    
    in \case
      ParallelBoth idxa idxb -> idxStep (idxNext oa idxa, idxNext ob idxb)
      ParallelA idxa -> idxStep (idxNext oa idxa, Nothing)
      ParallelB idxb -> idxStep (Nothing, idxNext ob idxb)

data IndexLoop i = IndexLoop i
  deriving (Generic, Eq, NFDataX)

instance StateIndex i => StateIndex (IndexLoop i) where
  type Outside (IndexLoop i) = Outside i

  idxInit o = IndexLoop $ idxInit o

  idxNext o (IndexLoop idx) = case idxNext o idx of
    Just idx' -> Just (IndexLoop idx')
    Nothing -> Just . IndexLoop $ idxInit o


data OutsideStatus
  = Block
  | NonBlock
  deriving (Generic, Eq, NFDataX)
instance CP.Default OutsideStatus where
  def = Block

data IndexSend = SendIdle | SendWork
  deriving (Generic, Eq, NFDataX)

instance StateIndex IndexSend where
  type Outside IndexSend = OutsideStatus

  idxInit _ = SendIdle

  idxNext o = \case
    SendIdle -> case o of
      Block -> Just SendIdle
      NonBlock -> Just SendWork
    SendWork -> case o of
      Block -> Just SendWork
      NonBlock -> Nothing

data IndexListen = ListenIdle | ListenWork
  deriving (Generic, Eq, NFDataX)

instance StateIndex IndexListen where
  type Outside IndexListen = OutsideStatus

  idxInit _ = ListenIdle

  idxNext o = \case
    ListenIdle -> case o of
      Block -> Just ListenIdle
      NonBlock -> Just ListenWork
    ListenWork -> case o of
      Block -> Just ListenWork
      NonBlock -> Nothing

data IndexRace a b = IndexRace a b
  deriving (Generic, Eq, NFDataX)

instance (StateIndex a, StateIndex b) =>
  StateIndex (IndexRace a b) where
  type Outside (IndexRace a b) = (Outside a, Outside b)

  idxInit (oa, ob) = IndexRace (idxInit oa) (idxInit ob)

  idxNext (oa, ob) (IndexRace idxa idxb) =
    case (idxNext oa idxa, idxNext ob idxb) of
      (Just idxa', Just idxb') -> Just $ IndexRace idxa' idxb'
      _ -> Nothing

-- | The `Connector`.
data Connector
  (p :: [Type]) -- ^ Ports and channels.
  (s :: Type)   -- ^ Inner state.
  (i :: Type)   -- ^ Outer state index space.
  (a :: Type)   -- ^ Result type.
  where
  -- | Pure action.
  Pure
    :: a
    -> Connector p s IndexUnit a

  -- | Binding two actions, blocking the latter until the former finishes.
  --
  -- The result state index space is the sum of the two actions' state
  -- index space.
  Bind
    :: ( StateIndex ia
       , StateIndex ib
       )
    => Connector p s ia a
    -> (a -> Connector p s ib b)
    -> Connector p s (IndexBind ia ib) b

  -- | Conditional branch. At most one branch would be performed at a time.
  Cond
    :: ( StateIndex ia
       , StateIndex ib
       )
    => Bool
    -> Connector p s ia r
    -> Connector p s ib r
    -> Connector p s (IndexCond ia ib) r

  -- | Perform two actions in parallel, blocking subsequent actions
  -- until both actions finish.
  --
  -- The result state index space is the product of the two actions'
  -- state index space.
  --
  -- @Note@: The behavior is different from `liftA2`.
  LiftP2
    :: ( StateIndex ia
       , StateIndex ib
       )
    => (a -> b -> c)
    -> Connector p s ia a
    -> Connector p s ib b
    -> Connector p s (IndexParallel ia ib) c

  -- | Perform an action forever
  Infloop
    :: StateIndex i
    => Connector p s i ()
    -> Connector p s (IndexLoop i) ()

  -- | Modify the inner register state.
  RegState
    :: (s -> (a, s))
    -> Connector p s IndexUnit a
  
  -- | Send a value with type `t`, to port `pt`, channel `ch`.
  Send
    :: Elem2 pt ch p (Channel t)
    => Proxy pt
    -> Proxy ch
    -> (s -> t)
    -> Connector p s IndexSend ()

  -- | Listen to multiple channels.
  Listen
    :: StateIndex i
    => Listener p s i
    -> Connector p s i ()


-- | Helper datatype for specifying channels to listen
data Listener (p :: [Type]) (s :: Type) (i :: Type) where
  Listen1
    :: Elem2 pt ch p (CoChannel t)
    => Proxy pt
    -> Proxy ch
    -> (Maybe t -> (s -> s))
    -> Listener p s IndexListen
  Listen2
    :: ( StateIndex ia
       , StateIndex ib
       )
    => Listener p s ia
    -> Listener p s ib
    -> Listener p s (IndexRace ia ib)



-- $exposed
--
-- The exposed functions mainly use Lens APIs, and specify
-- channels with type-level parameters.
--
-- As for `Connector p s`, the constraint `Monoid s` should
-- be always satisfied.
--
-- In practice, `s` is usually a record type where all its
-- fields are `Monoid`s like `First` or `Last`, since the
-- `Semigroup` and `Monoid` instances can be derived, and
-- the `Lens`es can be meanwhile built automatically.


(>>=)
  :: ( StateIndex ia
     , StateIndex ib
     )
  => Connector p s ia a
  -> (a -> Connector p s ib b)
  -> Connector p s (IndexBind ia ib) b
(>>=) = Bind

(>>)
  :: ( StateIndex ia
     , StateIndex ib
     )
  => Connector p s ia a
  -> Connector p s ib b
  -> Connector p s (IndexBind ia ib) b
(>>) x y = Bind x (const y)


pure :: a -> Connector p s IndexUnit a
pure = Pure

state :: (s -> (a, s)) -> Connector p s IndexUnit a
state = RegState

get :: Connector p s IndexUnit s
get = RegState (\x -> (x, x))

gets :: (s -> a) -> Connector p s IndexUnit a
gets f = RegState (\x -> (f x, x))

set :: s -> Connector p s IndexUnit ()
set x = RegState (\_ -> ((), x))

modify :: (s -> s) -> Connector p s IndexUnit ()
modify f = RegState (\x -> ((), f x))


-- | Send a value to a channel.
--
-- Sending to multiple channels can be expressed by
-- multiple `send`s in parallel.
send
  :: forall
     (pt :: Symbol) (ch :: Symbol)
     {t :: Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Elem2 pt ch p (Channel t)
     )
  => Getter s t
     -- ^ The data being sent must be derived from the register,
     -- and must NOT change before sent.
  -> Connector p s IndexSend ()
send l = Send (Proxy @pt) (Proxy @ch) (view l)

-- | Listen to a single channel.
listen1
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     {t :: Type} {a :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a)
     , CP.Applicative f
     , Elem2 pt1 ch1 p (CoChannel t)
     )
  => (t -> a)
     -- ^ Transform the raw data from the channel to the
     -- corresponding representation in the register.
  -> Setter' s (f a)
     -- ^ The setter of the corresponding register.
     -- In practice, each listened channel probably corresponds to
     -- separate parts of the register, while shared register parts
     -- are permitted for efficiency.
  -> Connector p s IndexListen ()
listen1 f1 l1 = Listen $
  Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (CP.pure . f1) t)

-- | Listen to 2 channels.
--
-- The channels are blocked after one or more channels' transfers
-- occur. If a transfer occur, the data is wrapped in `CP.pure` before
-- being set in the register. Otherwise `mempty` is used instead.
-- Therefore, the subsequential program can determine whether a
-- transfer has occured or not.
listen2
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     (pt2 :: Symbol) (ch2 :: Symbol)
     {t1 :: Type} {a1 :: Type}
     {t2 :: Type} {a2 :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a1)
     , Monoid (f a2)
     , CP.Applicative f
     , Elem2 pt1 ch1 p (CoChannel t1)
     , Elem2 pt2 ch2 p (CoChannel t2)
     )
  => (t1 -> a1)       -- ^ Transform the result from channel 1.
  -> Setter' s (f a1) -- ^ Setter 1.
  -> (t2 -> a2)       -- ^ Transform the result from channel 2.
  -> Setter' s (f a2) -- ^ Setter 2.
  -> Connector p s (IndexRace IndexListen IndexListen) ()
listen2 f1 l1 f2 l2 = Listen $ Listen2
  (Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (CP.pure . f1) t))
  (Listen1 (Proxy @pt2) (Proxy @ch2) (\t -> l2 .~ maybe mempty (CP.pure . f2) t))

-- | Listen to 4 channels.
--
-- `listenN` for arbitrary `N` can be probably implemented with TH.
-- However, for now these are enough.
listen4
  :: forall
     (pt1 :: Symbol) (ch1 :: Symbol)
     (pt2 :: Symbol) (ch2 :: Symbol)
     (pt3 :: Symbol) (ch3 :: Symbol)
     (pt4 :: Symbol) (ch4 :: Symbol)
     {t1 :: Type} {a1 :: Type}
     {t2 :: Type} {a2 :: Type}
     {t3 :: Type} {a3 :: Type}
     {t4 :: Type} {a4 :: Type}
     {f :: Type -> Type} {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , Monoid (f a1)
     , Monoid (f a2)
     , Monoid (f a3)
     , Monoid (f a4)
     , CP.Applicative f
     , Elem2 pt1 ch1 p (CoChannel t1)
     , Elem2 pt2 ch2 p (CoChannel t2)
     , Elem2 pt3 ch3 p (CoChannel t3)
     , Elem2 pt4 ch4 p (CoChannel t4)
     )
  => (t1 -> a1)       -- ^ Transform the result from channel 1.
  -> Setter' s (f a1) -- ^ Setter 1.
  -> (t2 -> a2)       -- ^ Transform the result from channel 2.
  -> Setter' s (f a2) -- ^ Setter 2.
  -> (t3 -> a3)       -- ^ Transform the result from channel 3.
  -> Setter' s (f a3) -- ^ Setter 3.
  -> (t4 -> a4)       -- ^ Transform the result from channel 4.
  -> Setter' s (f a4) -- ^ Setter 4.
  -> Connector p s
     (IndexRace
       (IndexRace IndexListen IndexListen)
       (IndexRace IndexListen IndexListen)
     )
     ()
listen4 f1 l1 f2 l2 f3 l3 f4 l4 = Listen $ Listen2
  (Listen2
    (Listen1 (Proxy @pt1) (Proxy @ch1) (\t -> l1 .~ maybe mempty (CP.pure . f1) t))
    (Listen1 (Proxy @pt2) (Proxy @ch2) (\t -> l2 .~ maybe mempty (CP.pure . f2) t))
  )
  (Listen2
    (Listen1 (Proxy @pt3) (Proxy @ch3) (\t -> l3 .~ maybe mempty (CP.pure . f3) t))
    (Listen1 (Proxy @pt4) (Proxy @ch4) (\t -> l4 .~ maybe mempty (CP.pure . f4) t))
  )

-- | Conditional branch.
cond
  :: forall
     {r :: Type}
     {ia :: Type} {ib :: Type}
     {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , StateIndex ia
     , StateIndex ib
     )
  => Bool
  -> Connector p s ia r
  -> Connector p s ib r
  -> Connector p s (IndexCond ia ib) r
cond = Cond

-- | Alias for `LiftP2` when the results are discarded.
parallel
  :: forall
     {a :: Type} {b :: Type}
     {ia :: Type} {ib :: Type}
     {p :: [Type]} {s :: Type}
  .  ( Monoid s
     , StateIndex ia
     , StateIndex ib
     )
  => Connector p s ia a -- ^ The result is discarded.
  -> Connector p s ib b -- ^ The result is discarded.
  -> Connector p s (IndexParallel ia ib) ()
parallel = LiftP2 (const (const ())) 

-- | Perform an action infinitely.
infloop
  :: forall
     {p :: [Type]} {s :: Type} {i :: Type}
  .  ( Monoid s
     , StateIndex i
     )
  => Connector p s i () -- ^ Action for an iteration.
  -> Connector p s (IndexLoop i) ()
infloop = Infloop
