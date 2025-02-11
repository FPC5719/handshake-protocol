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

class Tuplifiable r t where
  tuplify :: Record r -> t
  untuplify :: t -> Record r

instance Tuplifiable '[] () where
  tuplify (Record HNil) = ()
  untuplify () = Record HNil

instance (Tuplifiable r t, Tuplifiable rs ts) =>
  Tuplifiable (Tagged s (Record r) ': rs) (t, ts) where
  tuplify (Record (HCons (Tagged r) rs))
    = (tuplify r, tuplify (Record rs))
  untuplify (t, ts) =
    let Record rts = untuplify ts
    in Record (HCons (Tagged (untuplify t)) rts)

instance (Tuplifiable rs ts) =>
  Tuplifiable (Tagged s a ': rs) (a, ts) where
  tuplify (Record (HCons (Tagged a) rs)) = (a, tuplify (Record rs))
  untuplify (t, ts) =
    let Record rts = untuplify ts
    in Record (HCons (Tagged t) rts)

tumap
  :: ( Tuplifiable r1 t1
     , Tuplifiable r2 t2
     )
  => (Record r1 -> Record r2)
  -> (t1 -> t2)
tumap fn = tuplify . fn . untuplify

tumapF
  :: ( Tuplifiable r1 t1
     , Tuplifiable r2 t2
     , Functor f
     )
  => (f (Record r1) -> f (Record r2))
  -> (f t1 -> f t2)
tumapF fn = (tuplify <$>) . fn . (untuplify <$>)

qx
  :: forall (sym :: Symbol) r s a
  .  HLens sym r s s a a
  => Lens' (r s) a
qx = hLens (Label @sym)
