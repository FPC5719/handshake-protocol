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
{-
type MyPorts =
  '[ "In"  ::~ Input Value
   , "Out" ::~ 'Rec
     '[ "Data"  ::~ Output Value
      , "Ready" ::~ Input Bool
      ]
   ]
-}
data MyInput = MyInput
  { _myIn :: Unsigned 8
  , _myOutReady :: Bool
  }
makeLenses 'MyInput

data MyOutput = MyOutput
  { _myOutData :: First Value
  }
  deriving (Generic, Default, NFDataX)
  deriving Semigroup via GenericSemigroup MyOutput
  deriving Monoid via GenericMonoid MyOutput
makeLenses 'MyOutput

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
  ( cond ((== 42) . (view $ myIn) . fst) $
    ( embedS $ \_ -> do
        modify $ count %~ pure . (maybe 0 (+ 1)) . getFirst
        pure mempty
    ) &>
    ( rmap snd . loop (isJust . getFirst . fst) $
      embedS $ \inp -> do
        cnt <- use count
        pure $ if inp ^. myOutReady
          then (mempty , mempty)
          else (pure (), mempty & myOutData .~ cnt)
    )
  )

myTop'
  :: HiddenClockResetEnable dom
  => Signal dom MyInput
  -> Signal dom MyOutput
myTop' = case myFSM of
  FSM' (FSM f) -> flip mealy (mempty, initial) $
    \(r, s) inp ->
      let (o, r', ms) = f (Just inp, r, s)
      in ((r', fromMaybe initial ms), o)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System MyInput
  -> Signal System MyOutput
topEntity clk rst en =
  withClockResetEnable clk rst en myTop'
