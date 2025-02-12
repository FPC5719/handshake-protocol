{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

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

type family Tuplify a where
  Tuplify (Record '[]) = ()
  Tuplify (Record (Tagged s a ': rs)) =
    (Tuplify a, Tuplify (Record rs))
  Tuplify a = a

class Tuplifiable a where
  tuplify :: a -> Tuplify a
  untuplify :: Tuplify a -> a

instance Tuplifiable (Record '[]) where
  tuplify (Record HNil) = ()
  untuplify () = Record HNil

instance (Tuplifiable a, Tuplifiable (Record rs)) =>
  Tuplifiable (Record (Tagged s a ': rs)) where
  tuplify (Record (HCons (Tagged a) rs))
    = (tuplify a, tuplify (Record rs))
  untuplify (t, ts) =
    let Record rts = untuplify ts
    in Record (HCons (Tagged (untuplify t)) rts)

instance Tuplifiable () where
  tuplify = id
  untuplify = id

instance Tuplifiable Bool where
  tuplify = id
  untuplify = id

instance Tuplifiable (Unsigned n) where
  tuplify = id
  untuplify = id

instance Tuplifiable (Signed n) where
  tuplify = id
  untuplify = id

instance Functor f => Tuplifiable (f a) where
  tuplify = id
  untuplify = id

tumap
  :: ( Tuplifiable r1
     , Tuplifiable r2
     )
  => (r1 -> r2)
  -> (Tuplify r1 -> Tuplify r2)
tumap fn = tuplify . fn . untuplify

tumapF
  :: ( Tuplifiable r1
     , Tuplifiable r2
     , Functor f
     )
  => (f r1 -> f r2)
  -> (f (Tuplify r1) -> f (Tuplify r2))
tumapF fn = (tuplify <$>) . fn . (untuplify <$>)

qx
  :: forall (sym :: Symbol) r s a
  .  HLens sym r s s a a
  => Lens' (r s) a
qx = hLens (Label @sym)
