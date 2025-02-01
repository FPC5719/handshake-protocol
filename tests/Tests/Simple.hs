{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}
module Tests.Simple where

import Clash.Prelude

import Protocol.Channel
import qualified Protocol.Connector as C
import Protocol.Internal.Util

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic

type Counter a = C.WrapConnector
  '[ Slave  (Handshake (Unsigned 8)) `Tagged2` "In"
   , Master (Handshake (Unsigned 8)) `Tagged2` "Out" ]
  CounterState
  a

data CounterState = CounterState
  { _csInput :: First (Unsigned 8)
  , _csCount :: First (Unsigned 8)
  }
  deriving (Generic, Default, NFDataX)
  deriving Semigroup via GenericSemigroup CounterState
  deriving Monoid via GenericMonoid CounterState
makeLenses 'CounterState

simple :: Counter ()
simple = C.WrapConnector $ C.infloop $ C.do
  C.listen1 @"In" @"Data" id csInput
  inp <- C.gets (view csInput)
  if inp == pure 42
    then C.modify $ csCount %~ First . maybe (Just 0) (Just . (+1)) . getFirst
    else C.pure ()
  C.send @"Out" @"Data" $ csCount . to (fromMaybe 0 . getFirst)
