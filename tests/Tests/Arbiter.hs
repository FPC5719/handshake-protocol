{-# LANGUAGE DerivingVia #-}
module Tests.Arbiter where

import Clash.Prelude

import Protocol.Channel
import Protocol.Connector
import Protocol.Internal.Util

import Control.Lens(makeLenses, to, use)
import Control.Monad.Extra
import Control.Monad.State.Class()
import Data.Maybe
import Data.Monoid
import Data.Monoid.Generic

type Addr  = Unsigned 32
type Value = Unsigned 32
type Back  = Unsigned 2
type Port  = AXILite Addr Value Back

type Arbiter a = Connector
  '[ Slave  Port `Tagged2` "In1"
   , Slave  Port `Tagged2` "In2"
   , Master Port `Tagged2` "Out"
   ]
  ArbiterState
  a

data ArbiterState = ArbiterState
  { _aw1 :: First Addr
  , _aw2 :: First Addr
  , _w1  :: First Value
  , _w2  :: First Value
  , _ar1 :: First Addr
  , _ar2 :: First Addr
  , _b   :: First Back
  , _r   :: First Value
  }
  deriving (Generic, Default)
  deriving Semigroup via GenericSemigroup ArbiterState
  deriving Monoid    via GenericMonoid    ArbiterState
makeLenses 'ArbiterState

arbiter :: Arbiter ()
arbiter = do
  parallel
    (infloop $ do
        listen4
          @"In1" @"AW"  @"In1" @"W"  @"In2" @"AW"  @"In2" @"W"
          pure aw1      pure w1      pure aw2      pure w2
        whenM ((||) <$> got aw1 <*> got w1) $ do
          parallel
            (unlessM (got aw1) $ listen1 @"In1" @"AW" pure aw1)
            (unlessM (got w1)  $ listen1 @"In1" @"W"  pure w1)
          parallel
            (send @"Out" @"AW" (ret aw1))
            (send @"Out" @"W"  (ret w1))
          listen1 @"Out" @"B"  pure b
          send    @"In1" @"B"  (ret b)
        whenM ((||) <$> got aw2 <*> got w2) $ do
          parallel
            (unlessM (got aw2) $ listen1 @"In2" @"AW" pure aw2)
            (unlessM (got w2)  $ listen1 @"In2" @"W"  pure w2)
          parallel
            (send @"Out" @"AW" (ret aw2))
            (send @"Out" @"W"  (ret w2))
          listen1 @"Out" @"B"  pure b
          send    @"In2" @"B"  (ret b)
    )
    (infloop $ do
        listen2
          @"In1" @"AR"  @"In2" @"AR"
          pure ar1      pure ar2
        whenM (got ar1) $ do
          send    @"Out" @"AR" (ret ar1)
          listen1 @"Out" @"R"  pure r
          send    @"In1" @"R"  (ret r)
        whenM (got ar2) $ do
          send    @"Out" @"AR" (ret ar2)
          listen1 @"Out" @"R"  pure r
          send    @"In2" @"R"  (ret r)
    )
    where
      got l = isJust . getFirst <$> use l
      ret l = l . to (fromMaybe 0 . getFirst)
