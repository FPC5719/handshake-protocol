module Protocol.Channel where

data   Channel (a :: Type)
data CoChannel (a :: Type)

class IsChannel ch where
  type Master ch :: [Type]
  type Slave  ch :: [Type]
