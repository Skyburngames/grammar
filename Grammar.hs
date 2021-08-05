-- Allow ourselves to write Text literals.
{-# LANGUAGE DeriveGeneric #-} -- must be set BEFORE 'module Main where'
{-# LANGUAGE OverloadedStrings #-} -- must be set BEFORE 'module Main where'


{-
============================= GRAMMAR ===========================================
-}
module Grammar
(
    -- ARCHITECTURE --
    Game(..),
    GameType(..),
    Level(..),
    Room(..),
    Rule(..),
    RoomConnector(..),
    Entity(..),
    EntityType(..),
    ObjectId(..),
    Tile(..),
    Grid(..),
    TileType(..),
    -- HELPER CLASSES --
    Position(..),
    Vector2(..),
    InputData(..),
    OriginalData(..),
    TileBuilder(..),
    TileCondition(..),
    TileModifier(..),
    ChanceCalculation(..),
    createTile,
    getTile,
    getTileRelative,
    getPositionRelative,
    isPosition,
    isPositionInGrid,
    compareTileTypes,
    generateTiles,
    getGridHeight,
    getGridWidth,
    getMax,
    clamp,
    clamp01,
    toInt,
    toFloat
) where

-- ============================= IMPORTS =============================
import GHC.Generics
import System.Random


-- ==================================== HELPER CLASSES and TYPES ======================================
data Position = Position {
    x:: Int,
    y:: Int
}deriving (Show)

type Vector2 = (Int, Int)

-- InputData: OriginalData is always the same for each call provided by the caller, Position, Tile and StdGen are UNIQUE for each call!
type InputData = (OriginalData,Position,Tile,StdGen) -- the data that is always fed to an TileCondition and TileBuilder
type OriginalData = (Grid, StdGen)
type TileBuilder = InputData->Tile

type TileCondition = InputData->Bool

type TileModifier = (Tile->Tile)
data ChanceCalculation = Highest| Cumulative | Average deriving (Show)


-- ============================= Architecture =============================
data Game = Game {
    levels :: [Level],
    gameType :: GameType
} deriving (Show, Generic)


data GameType = FPS| TopDown deriving (Show, Generic)


data Level = Level {
    name:: String,
    rooms:: [Room]
} deriving (Show, Generic)


data Room = Room {
    roomId:: ObjectId,
    grid:: Grid
} deriving (Show, Generic)

data Grid = Grid {
    tiles:: [[Tile]]
} deriving (Show, Generic)


data Tile = Tile {
    tileType:: TileType,
    entities:: [Entity]
} deriving (Show, Generic)


data TileType = Solid | Open deriving (Show, Generic)

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

-- ======================================= HELPER FUNCTIONS ===========================================
getMax::(Ord a)=>[a]->a
getMax [x] = x
getMax (x:xs) = max x (getMax xs)

clamp::(Ord a)=>a->a->a->a
clamp value minV maxV = min maxV (max value minV)

clamp01::Float->Float
clamp01 value = clamp value 0 1

toInt ::Float->Int
toInt x = round x

toFloat::Int->Float
toFloat x = fromIntegral x :: Float




-- ================================== Grid generation ==================================
generateTiles::Int->Int->[[Tile]]
generateTiles _ 0 = [[]]
generateTiles width height = if(height == 1) then [generateTilesRow width] else
    generateTilesRow width : generateTiles width (height-1)
    where {
        generateTilesRow::Int->[Tile];
        generateTilesRow 0 = [];
        generateTilesRow x = (createTile Open []): generateTilesRow (x-1)
    }


-- ==================================== GET GRID DATA ======================================
getTile::Grid->Position->Tile -- idea: make faster(?) by using 2x concate and then calc the index with divide/modulo
getTile grid position = findInTilesPosition allTiles position
    where {
        allTiles = getTilesPosition grid;
        posX = (x position);
        posY = (y position);

        findInTilesPosition::[(Position, Tile)]->Position->Tile;
        findInTilesPosition (x:[]) findPos = snd x; --NOTE: if position could not be found the last tile is always returned
        findInTilesPosition (x:xs) findPos = if(isPosition (fst x) findPos) then snd x else findInTilesPosition xs findPos
    }

-- Returns the tile relative to originalPos
getTileRelative::Grid->Position->Vector2->Tile
getTileRelative grid originalPos offset = getTile grid (getPositionRelative originalPos offset)

getPositionRelative::Position->Vector2->Position
getPositionRelative originalPos offset = Position ((x originalPos) + fst offset) ((y originalPos) + snd offset)


getTilesPosition::Grid->[(Position, Tile)]
getTilesPosition grid = concat [[((Position x y), tile)|(x, tile) <- zip[0..] row]|(y, row) <- zip[0..] (tiles grid)]

getGridWidth::Grid->Int
getGridWidth grid = length (head (tiles grid))

getGridHeight::Grid->Int
getGridHeight grid = length (tiles grid)


compareTileTypes::TileType->TileType->Bool;
compareTileTypes Solid Solid = True;
compareTileTypes Open Open = True;
compareTileTypes _ _ = False

isPosition::Position->Position->Bool
isPosition pos1 pos2 = if ((x pos1) == (x pos2) && (y pos1) == (y pos2)) then True else False

isPositionInGrid::Grid->Position->Bool
isPositionInGrid grid pos = True

-- ============================ Smart constructors =========================================
createTile::TileType->[Entity]->Tile
createTile tileType entitiesTile = Tile tileType entitiesTile



-- =================================== NOT USED ================================
data Rule = Rule deriving (Show)

data RoomConnector = RoomConnector {
    -- roomConnectorType:: RoomConnectorType,
    roomId1:: ObjectId,
    roomId2:: ObjectId
} deriving (Show, Generic)
