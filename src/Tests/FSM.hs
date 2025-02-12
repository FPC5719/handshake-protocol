{-# LANGUAGE DerivingVia #-}

module Tests.FSM where

import Protocol.FSM
import Protocol.Interface

import Clash.Prelude
import Clash.Annotations.TH
import Control.Lens
import Control.Monad.State.Strict
import Data.HList
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic

type Value = Unsigned 8

type MyPorts =
  '[ "In"  ::~ Input Value
   , "Out" ::~ 'Rec
     '[ "Data"  ::~ Output Value
      , "Ready" ::~ Input Bool
      ]
   ]

type MyInput  = Record (FInput  MyPorts)
type MyOutput = Record (FOutput MyPorts)

data MyState = MyState
  { _count :: First Value
  }
  deriving (Generic, Default, NFDataX)
  deriving Semigroup via GenericSemigroup MyState
  deriving Monoid via GenericMonoid MyState
makeLenses 'MyState

myFSM :: FSM' MyState MyInput MyOutput
myFSM = FSM' $
  loop (const False) $
  ( cond ((== 42) . (view $ qx @"In")) $
    ( embedS $ \_ -> do
        modify $ count %~ pure . (maybe 0 (+ 1)) . getFirst
        pure mempty
    ) &>
    ( rmap snd . loop (isJust . getFirst . fst) $
      embedS $ \inp -> do
        cnt <- use count
        pure $ if inp ^. qx @"Out" . qx @"Ready"
          then (mempty , mempty)
          else (pure (), mempty & qx @"Out" . qx @"Data" .~ cnt)
    )
  )

type TuInput = (Unsigned 8, ((Bool, ()), ()))
type TuOutput = ((First (Unsigned 8), ()), ())

myTop'
  :: HiddenClockResetEnable dom
  => Signal dom TuInput
  -> Signal dom TuOutput
myTop' = tumapF $ case myFSM of
  FSM' (FSM f) -> flip mealy (mempty, initial) $
    \(r, s) inp ->
      let (o, r', ms) = f (Just inp, r, s)
      in ((r', fromMaybe initial ms), o)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System TuInput
  -> Signal System TuOutput
topEntity clk rst en =
  withClockResetEnable clk rst en myTop'
