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
