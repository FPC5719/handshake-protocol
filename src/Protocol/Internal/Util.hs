{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Protocol.Internal.Util where

import Clash.Prelude



data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)


infixr 4 :%, :%%

pattern (:%) :: t -> HList ts -> HList (Tagged t s ': ts)
pattern a :% b <- HCons (Tagged a) b
  where a :% b = HCons (Tagged a) b
{-# COMPLETE (:%) #-}

pattern (:%%) :: HList ts -> HList tss -> HList (Tagged2 ts s ': tss)
pattern a :%% b <- HCons (Tagged2 a) b
  where a :%% b = HCons (Tagged2 a) b
{-# COMPLETE (:%%) #-}



class Tuplify a where
  type TupleRep a
  tuplify   :: a -> TupleRep a
  untuplify :: TupleRep a -> a

instance Tuplify (HList '[]) where
  type TupleRep (HList '[]) = ()
  tuplify HNil = ()
  untuplify () = HNil

instance Tuplify (HList ts) => Tuplify (HList ((t `Tagged` s) ': ts)) where
  type TupleRep (HList ((t `Tagged` s) ': ts)) = (t, TupleRep (HList ts))
  tuplify (x :% xs) = (x, tuplify xs)
  untuplify (x, xs) = x :% untuplify xs

tumap
  :: ( Tuplify a
     , Tuplify b
     )
  => (a -> b)
  -> (TupleRep a -> TupleRep b)
tumap f = tuplify . f . untuplify


class Tuplify2 a where
  type TupleRep2 a
  tuplify2   :: a -> TupleRep2 a
  untuplify2 :: TupleRep2 a -> a

instance Tuplify2 (HList '[]) where
  type TupleRep2 (HList '[]) = ()
  tuplify2 HNil = ()
  untuplify2 () = HNil

instance (Tuplify2 (HList tss), Tuplify (HList ts)) =>
  Tuplify2 (HList ((ts `Tagged2` s) ': tss)) where
  type TupleRep2 (HList ((ts `Tagged2` s) ': tss)) =
    (TupleRep (HList ts), TupleRep2 (HList tss))
  tuplify2 (xs :%% xss) = (tuplify xs, tuplify2 xss)
  untuplify2 (xs, xss) = untuplify xs :%% untuplify2 xss

tumap2
  :: ( Tuplify2 a
     , Tuplify2 b
     )
  => (a -> b)
  -> (TupleRep2 a -> TupleRep2 b)
tumap2 f = tuplify2 . f . untuplify2



data Tagged  (a :: Type)   (s :: Symbol) = Tagged a
data Tagged2 (a :: [Type]) (s :: Symbol) = Tagged2 (HList a)

class ListTagged ts
instance ListTagged '[]
instance ListTagged ts => ListTagged ((t `Tagged` s) ': ts)

class ListTagged2 ts
instance ListTagged2 '[]
instance (ListTagged t, ListTagged2 ts) => ListTagged2 ((t `Tagged2` s) ': ts)


class Elem (s :: Symbol) (ts :: [Type]) (tr :: Type)
instance                   ListTagged ts                => Elem s ((tr `Tagged` s ) ': ts) tr
instance {-# OVERLAPS #-} (ListTagged ts, Elem s ts tr) => Elem s ((t  `Tagged` s') ': ts) tr

class Elem2 (p :: Symbol) (q :: Symbol) (ts :: [Type]) (tr :: Type)
instance                  (ListTagged2 ts, Elem q t tr    ) => Elem2 p q ((t `Tagged2` p ) ': ts) tr
instance {-# OVERLAPS #-} (ListTagged2 ts, Elem2 p q ts tr) => Elem2 p q ((t `Tagged2` p') ': ts) tr
