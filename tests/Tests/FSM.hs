{-# LANGUAGE DerivingVia #-}

module Tests.FSM where

import Protocol.FSM
import Protocol.Interface

import Clash.Prelude
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
  , _stop  :: First Bool
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
    ( loop (fromMaybe False . getFirst . view stop) $
      embedS $ \inp -> do
        let ready = inp ^. qx @"Out" . qx @"Ready"
        cnt <- use count
        if ready
          then pure $ mempty
          else do
          modify $ stop .~ pure True
          pure $ mempty & qx @"Out" . qx @"Data" .~ cnt
    )
  )
