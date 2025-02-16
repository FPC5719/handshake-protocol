module Protocol.AXILite where

import Protocol.FSM
import Protocol.Handshake

import Clash.Prelude
import Control.Lens
import Data.Monoid
import Data.Monoid.Generic



data AXILiteRequest a d m b = AXILiteRequest
  { _axivAR :: Channel a
  , _axirR  :: Ready
  , _axivAW :: Channel a
  , _axivW  :: Channel (d, m)
  , _axirB  :: Ready
  }
  deriving (Generic, NFDataX)
  deriving Semigroup via GenericSemigroup (AXILiteRequest a d m b)
  deriving Monoid    via GenericMonoid    (AXILiteRequest a d m b)
makeLenses ''AXILiteRequest

data AXILiteResponse a d m b = AXILiteResponse
  { _axirAR :: Ready
  , _axivR  :: Channel d
  , _axirAW :: Ready
  , _axirW  :: Ready
  , _axivB  :: Channel b
  }
  deriving (Generic, NFDataX)
  deriving Semigroup via GenericSemigroup (AXILiteResponse a d m b)
  deriving Monoid    via GenericMonoid    (AXILiteResponse a d m b)
makeLenses ''AXILiteResponse

data SimpleAXILiteRequest a d m b = SimpleAXILiteRequest
  { _saxivReq :: Channel (Either a (a, (d, m)))
  , _saxirRes :: Ready
  }
  deriving (Generic, NFDataX)
  deriving Semigroup via GenericSemigroup (SimpleAXILiteRequest a d m b)
  deriving Monoid    via GenericMonoid    (SimpleAXILiteRequest a d m b)
makeLenses ''SimpleAXILiteRequest

data SimpleAXILiteResponse a d m b = SimpleAXILiteResponse
  { _saxirReq :: Ready
  , _saxivRes :: Channel (Either d b)
  }
  deriving (Generic, NFDataX)
  deriving Semigroup via GenericSemigroup (SimpleAXILiteResponse a d m b)
  deriving Monoid    via GenericMonoid    (SimpleAXILiteResponse a d m b)
makeLenses ''SimpleAXILiteResponse


data ExtendState a d m b = ExtendState
  { _exstSReq :: First (Either a (a, (d, m)))
  , _exstAXIR :: First d
  , _exstAXIW :: First b
  }
  deriving (Generic, NFDataX)
  deriving Semigroup via GenericSemigroup (ExtendState a d m b)
  deriving Monoid    via GenericMonoid    (ExtendState a d m b)
makeLenses ''ExtendState

extendAXI
  :: ( IsFSM
       (ExtendState a d m b)
       s
       (SimpleAXILiteRequest a d m b, AXILiteResponse a d m b)
       (SimpleAXILiteResponse a d m b, AXILiteRequest a d m b)
     , s ~ (Either () (Maybe (Either (Either (Either () ()) ()) (Either (Either (Maybe (), Maybe ()) ()) ()))))
     )
  => FSM
     (ExtendState a d m b)
     s
     (SimpleAXILiteRequest a d m b, AXILiteResponse a d m b)
     (SimpleAXILiteResponse a d m b, AXILiteRequest a d m b)
extendAXI =
  ( listen (_1 . saxivReq) (_1 . saxirReq) exstSReq ) &>
  ( condCase (forceJust . getFirst . view exstSReq)
    ( send   (_2 . _2 . axirAR)   (_2 . axivAR)   (exstSReq . to (forceLeft . forceJust . getFirst)) &>
      listen (_2 . _2 . axivR)    (_2 . axirR)    exstAXIR &>
      send   (_2 . _1 . saxirRes) (_1 . saxivRes) (exstAXIR . to (Left . forceJust . getFirst))
    )
    ( ( send (_2 . _2 . axirAW)   (_2 . axivAW)   (exstSReq . to (forceRight . forceJust . getFirst) . _1) &|
        send (_2 . _2 . axirW)    (_2 . axivW)    (exstSReq . to (forceRight . forceJust . getFirst) . _2)  
      ) &>
      listen (_2 . _2 . axivB)    (_2 . axirB)    exstAXIW &>
      send   (_2 . _1 . saxirRes) (_1 . saxivRes) (exstAXIW . to (Right . forceJust . getFirst))
    )
  )
  where
    forceJust :: Maybe a -> a
    forceJust (Just x) = x
    forceLeft :: Either a b -> a
    forceLeft (Left x) = x
    forceRight :: Either a b -> b
    forceRight (Right x) = x
