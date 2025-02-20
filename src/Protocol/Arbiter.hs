module Protocol.Arbiter where

import Protocol.FSM
import Protocol.Handshake

import Clash.Prelude
import Control.Lens hiding (Index)
import Data.Monoid

arbiter
  :: ( Eq a, Monoid a
     , Eq b, Monoid b
     )
  => ((BitVector n, s) -> (Maybe (Index n), s))
  -> FSM' s (Vec n a, b) (Vec n b, a)
arbiter f = undefined
