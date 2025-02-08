module Tests.FSM where

import Protocol.FSM
import Protocol.Interface

import Clash.Prelude
import Control.Lens
import Data.Monoid

type Value = Unsigned 8

type MyPorts =
  '[ "In"  ::~ Input Value
   , "Out" ::~ 'Rec
     '[ "Data"  ::~ Output Value
      , "Ready" ::~ Input Bool
      ]
   ]

data MyState = MyState
  { _count :: Value }
{-
myFSM :: FSM MyState s (Input MyPorts) (Output MyPorts)
myFSM =
  loop (const False) $
  ( cond ((== 42) . (view $ rl @"In")) $
    ( modify $ count .~ (+ 1) ) &>
    ( loop fst $
      ()
    )
  )
-}
