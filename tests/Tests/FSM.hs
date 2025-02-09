{-# LANGUAGE DerivingVia #-}

module Tests.FSM where

import Protocol.FSM
import Protocol.Interface

import Clash.Prelude
import Control.Lens
import Control.Monad.State.Strict
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
  , _stop  :: First Bool }
  deriving (Generic, Default)
  deriving Semigroup via GenericSemigroup MyState
  deriving Monoid via GenericMonoid MyState
makeLenses 'MyState

myFSM
  :: FSM
     MyState
     s
     (HList (FInput MyPorts))
     (HList (FOutput MyPorts))
myFSM =
  loop (const False) $
  ( cond ((== 42) . (view $ rl @"In")) $
    ( embed . modify $ count .~ (+ 1) ) &>
    ( loop (fromMaybe False . getFirst . view stop) $
      embed $ do
        ready <- view $ rl @"Out" . rl @"Ready"
        cnt <- use count
        if ready
          then pure $ mempty
          else do
          modify $ stop .~ pure True
          pure $ mempty & (rl @"Out" . rl @"Data") .~ pure cnt
    )
  )

