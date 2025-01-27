{-# LANGUAGE DerivingVia #-}
module Tests.Simple where

import Clash.Prelude

import Protocol.Channel
import Protocol.Connector
import Protocol.Internal.Util

import Control.Lens
import Control.Monad.State.Class
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic

type Counter a = Connector
  '[ Slave  (Handshake (Unsigned 8)) `Tagged2` "In"
   , Master (Handshake (Unsigned 8)) `Tagged2` "Out" ]
  CounterState
  a

data CounterState = CounterState
  { _csInput :: First (Unsigned 8)
  , _csCount :: First (Unsigned 8)
  }
  deriving (Generic, Default)
  deriving Semigroup via GenericSemigroup CounterState
  deriving Monoid via GenericMonoid CounterState
makeLenses 'CounterState

simple :: Counter ()
simple = infloop $ do
  listen1 @"In" @"Data" pure csInput
  inp <- use csInput
  if inp == pure 42
    then modify $ csCount %~ First . maybe (Just 0) (Just . (+1)) . getFirst
    else pure ()
  send @"Out" @"Data" $ csCount . to (fromMaybe 0 . getFirst)
