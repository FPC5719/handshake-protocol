{-# LANGUAGE AllowAmbiguousTypes #-}

module Protocol.Interface where

import Clash.Prelude

import Control.Lens
import Data.HList
import Data.Monoid

data ITerm
  = Input Type
  | Output Type
  | Rec [ISym]

data ISym = (::~) Symbol ITerm

type family FInput (t :: [ISym]) :: [Type] where
  FInput '[] = '[]
  FInput ((s ::~ Input a) ': ts) = Tagged s a ': FInput ts
  FInput ((s ::~ Output a) ': ts) = FInput ts
  FInput ((s ::~ Rec rs) ': ts) = Tagged s (Record (FInput rs)) ': FInput ts

type family FOutput (t :: [ISym]) :: [Type] where
  FOutput '[] = '[]
  FOutput ((s ::~ Output a) ': ts) = Tagged s (First a) ': FOutput ts
  FOutput ((s ::~ Input a) ': ts) = FOutput ts
  FOutput ((s ::~ Rec rs) ': ts) = Tagged s (Record (FOutput rs)) ': FOutput ts

qx
  :: forall (sym :: Symbol) r s a
  .  HLens sym r s s a a
  => Lens' (r s) a
qx = hLens (Label @sym)
