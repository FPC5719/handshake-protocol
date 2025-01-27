module Protocol.Channel where

import Clash.Prelude

import Protocol.Internal.Util

data   Channel (a :: Type)
data CoChannel (a :: Type)

class IsChannel ch where
  type Master ch :: [Type]
  type Slave  ch :: [Type]

data Handshake (a :: Type)
instance IsChannel (Handshake a) where
  type Master (Handshake a) =
    '[ Channel a `Tagged` "Data" ]
  type Slave (Handshake a) =
    '[ CoChannel a `Tagged` "Data" ]

data AXILite (addr :: Type) (item :: Type) (back :: Type)
instance IsChannel (AXILite addr item back) where
  type Master (AXILite addr item back) =
    '[   Channel addr `Tagged` "AW"
     ,   Channel item `Tagged` "W"
     , CoChannel back `Tagged` "B"
     ,   Channel addr `Tagged` "AR"
     , CoChannel item `Tagged` "R"
     ]
  type Slave (AXILite addr item back) =
    '[ CoChannel addr `Tagged` "AW"
     , CoChannel item `Tagged` "W"
     ,   Channel back `Tagged` "B"
     , CoChannel addr `Tagged` "AR"
     ,   Channel item `Tagged` "R"
     ]
