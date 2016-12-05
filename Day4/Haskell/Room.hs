module Room where

type RoomName =
    String

type RoomSector =
    Int

type SecurityChecksum =
    String

data Room = Room
    { _name :: RoomName
    , _sector :: RoomSector
    , _checksum :: SecurityChecksum
    } deriving (Show)
