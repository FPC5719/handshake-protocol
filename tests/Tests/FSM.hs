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

data MyState = MyState
  { _count :: First Value
  }
  deriving (Generic, Default, NFDataX)
  deriving Semigroup via GenericSemigroup MyState
  deriving Monoid via GenericMonoid MyState
makeLenses 'MyState

myFSM
  :: FSM'
     MyState
     (Record (FInput MyPorts))
     (Record (FOutput MyPorts))
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

myTop'
  :: HiddenClockResetEnable dom
  => Signal dom (Record (FInput MyPorts))
  -> Signal dom (Record (FOutput MyPorts))
myTop' = case myFSM of
  FSM' (FSM f) -> flip mealy (mempty, initial) $
    \(r, s) inp ->
      let (o, r', ms) = f (Just inp, r, s)
      in ((r', fromMaybe initial ms), o)

myTop
  :: ( Tuplifiable (FInput MyPorts) tin
     , Tuplifiable (FOutput MyPorts) tout
     )
  => "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "en" ::: Enable System
  -> "input" ::: Signal System tin
  -> "output" ::: Signal System tout
myTop clk rst en = withClockResetEnable clk rst en (tumapF myTop')
makeTopEntity 'myTop
