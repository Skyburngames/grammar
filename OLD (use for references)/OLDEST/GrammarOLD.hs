-- Allow ourselves to write Text literals.
{-# LANGUAGE DeriveGeneric #-} -- must be set BEFORE 'module Main where'
{-# LANGUAGE OverloadedStrings #-} -- must be set BEFORE 'module Main where'


{-
============================= GRAMMAR ===========================================
-}
module Grammar  
(   Game(..),
    GameType(..),
    Level(..),
    Room(..),
    Rule(..),
    RoomConnector(..),
    Entity(..),
    EntityType(..),
    ObjectId(..),
    -- GridPosition(..),
    Tile(..),
    Grid(..),
    TileType(..),
    createTile,
    Position(..),
    isPosition
    -- tilePosX,
    -- tilePosY
) where 

-- ============================= IMPORTS =============================
import GHC.Generics
import System.Random


-- ============================= Architecture =============================
data Game = Game {
    levels :: [Level],
    gameType :: GameType
} deriving (Show, Generic)


data GameType = FPS| TopDown deriving (Show, Generic)


data Level = Level {
    name:: String,
    rooms:: [Room]
    -- startRoomId:: ObjectId,
    -- exitRoomId:: ObjectId,
    -- roomConnectors:: [RoomConnector],
    -- rules:: [Rule]
} deriving (Show, Generic)


data Room = Room {
    roomId:: ObjectId,
    grid:: Grid
} deriving (Show, Generic)

data Grid = Grid {
    tiles:: [[Tile]] --,
    --stdGen:: StdGen
} deriving (Show, Generic)

data Tile = Tile {
    -- tileGridPosition:: GridPosition,
    tileType:: TileType,
    entities:: [Entity]
} deriving (Show, Generic)


data TileType = Solid | Open deriving (Show, Generic)





-- data RoomConnectorType = Door | Vertical deriving (Show)


data Entity = 
    Entity {
        entityId:: ObjectId,
        entityType:: EntityType
    } deriving (Show, Generic)


data EntityType = Enemy| Powerup | Player deriving (Show, Generic)


-- IDS
-- newtype EntityId = EntityId Int deriving (Show)
-- newtype RoomId = RoomId Int deriving (Show)


newtype ObjectId = ObjectId {
    objectId :: Int
} deriving (Show, Generic)

-- ==================================== HELPER CLASSES ======================================
data Position = Position {
    x:: Int,
    y:: Int
}


-- ============================ Smart constructors =========================================
isPosition::Position->Position->Bool
isPosition pos1 pos2 = if ((x pos1) == (x pos2) && (y pos1) == (y pos2)) then True else False
{-
createTile::TileType->[Entity]->Tile
createTile tileType entitiesTile = Tile tileType entitiesTile

tilePosX::Tile->Int
tilePosX tile = x (tileGridPosition tile) 

tilePosY::Tile->Int
tilePosY tile = y (tileGridPosition tile) 
-}

createTile::TileType->[Entity]->Tile
createTile tileType entitiesTile = Tile tileType entitiesTile



-- ============================ GENERATE LEVEL =========================================


-- =================================== NOT USED ================================
data Rule = Rule deriving (Show)

data RoomConnector = RoomConnector {
    -- roomConnectorType:: RoomConnectorType,
    roomId1:: ObjectId,
    roomId2:: ObjectId
} deriving (Show, Generic)