{-# LANGUAGE FlexibleInstances #-}

module Protocol.Index where


import Clash.Prelude
import Data.Void

data MaybeIndex = I0 | I1

data IMaybe (i :: MaybeIndex) (a :: Type) where
  INothing :: IMaybe I0 a
  IJust :: a -> IMaybe I1 a

type family IBind p q where
  IBind (IMaybe I0 a) (IMaybe I0 b) = IMaybe I0 Void
  IBind (IMaybe I0 a) (IMaybe I1 b) = IMaybe I1 b
  IBind (IMaybe I1 a) (IMaybe I0 b) = IMaybe I1 a
  IBind (IMaybe I1 a) (IMaybe I1 b) = IMaybe I1 (Either a b)

type family ICond p q where
  ICond (IMaybe I1 a) (IMaybe I1 b) = IMaybe I1 (Either a b)

type family ILoop p where
  ILoop (IMaybe I1 a) = IMaybe I1 a

{-
class (NFDataX i, Eq i) => StateIndex i where
  idxInit :: i

data IndexZero
data IndexOne = IndexOne
  deriving (Generic, Eq, NFDataX)

instance StateIndex IndexOne where
  idxInit = IndexOne

instance (StateIndex a, StateIndex b) => StateIndex (Either a b) where
  idxInit = Left idxInit

type family IndexBind a b where
  IndexBind IndexZero x = x
  IndexBind x IndexZero = x
  IndexBind x y = Either x y

type family IndexCond a b where
  IndexCond IndexZero x = x
  IndexCond x IndexZero = x
  IndexCond x y = Either x y

type family IndexLoop a where
  IndexLoop IndexZero = IndexOne
  IndexLoop x = x
-}

{-
-- | State index.
class (NFDataX i, Eq i, Default (Outside i)) => StateIndex i where
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
instance Default a => Default (OutsideChoice a b) where
  def = ChoiceLeft def


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
instance Default OutsideStatus where
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
-}
