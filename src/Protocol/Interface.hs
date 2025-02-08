module Protocol.Interface where

import Clash.Prelude

import Data.Kind
import Data.Monoid
import GHC.TypeLits


data ITerm
  = Input Type
  | Output Type
  | Rec [ISym]

data ISym = (::~) Symbol ITerm

type family FInput (t :: [ISym]) :: [Type] where
  FInput '[] = '[]
  FInput ((s ::~ Input a) ': ts) = a ': FInput ts
  FInput ((s ::~ Output a) ': ts) = FInput ts
  FInput ((s ::~ Rec rs) ': ts) = HList (FInput rs) ': FInput ts

type family FOutput (t :: [ISym]) :: [Type] where
  FOutput '[] = '[]
  FOutput ((s ::~ Output a) ': ts) = (First a) ': FOutput ts
  FOutput ((s ::~ Input a) ': ts) = FOutput ts
  FOutput ((s ::~ Rec rs) ': ts) = HList (FOutput rs) ': FOutput ts

data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

type Test = 'Rec
  '[ "Addr" ::~ 'Input (Maybe (Unsigned 32)) 
   , "Out"  ::~ 'Rec
     '[ "Ready" ::~ 'Input Bool
      , "Data"  ::~ 'Output (Maybe (Unsigned 32))
      ]
   ]

