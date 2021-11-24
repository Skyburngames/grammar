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
    RoomedLevel(..),
    Room(..),
    RoomConnector(..),
    Rule(..),
    Entity(..),
    EntityType(..),
    ObjectId(..),
    Tile(..),
    Grid(..),
    TileType(..),
    Level(..),
    -- HELPER CLASSES --
    Position(..),
    Vector2(..),
    TileData(..),
    GridData(..),
    -- OriginalData(..),
    TileBuilder(..),
    TileCondition(..),
    TileModifier(..),
    ChanceCalculation(..),
    -- Alignment(..),
    createTile,
    getTile,
    getTileRelative,
    getPositionRelative,
    isPosition,
    -- isPositionInGrid,
    compareTileTypes,
    -- generateTiles,
    getGridHeight,
    getGridWidth,
    getMax,
    clamp,
    clamp01,
    toInt,
    toFloat,
    divideIntCeil,
    combineGrids,
    combineTiles,
    createRoom,
    compareObjectId,
    reverseVector2,
    getGridSize,
    positionToVector,
    findRoom
) where

-- ============================= IMPORTS =============================
import GHC.Generics
import System.Random


-- ==================================== HELPER CLASSES and TYPES ======================================
data Position = Position {
    x:: Int,
    y:: Int
}deriving (Show, Generic)

type Vector2 = (Int, Int)

type GridData = (Grid, StdGen)
-- TileData: OriginalData is always the same for each call provided by the caller, Position, Tile and StdGen are UNIQUE for each call!
type TileData = (Grid,Position,Tile,StdGen) -- the data that is always fed to an TileCondition and TileBuilder
-- type OriginalData = (Grid)
type TileBuilder = TileData->Tile

type TileCondition = TileData->Bool

type TileModifier = (Tile->Tile)
data ChanceCalculation = Highest| Cumulative | Average deriving (Show)

-- data Alignment = Horizontal | Vertical deriving (Show, Generic)


-- ============================= Architecture =============================

-- --------------------------------- LayoutLayer -----------------------------------------
data Game = Game {
    levels :: [Level],
    gameType :: GameType
} deriving (Show, Generic)

data GameType = FPS| TopDown deriving (Show, Generic)

data RoomedLevel = RoomedLevel {
    name:: String,
    rooms:: [Room],
    roomConnections:: [RoomConnector]
} deriving (Show, Generic)

data Room = Room {
    roomId:: ObjectId,
    grid:: Grid
} deriving (Show, Generic)

data RoomConnector = RoomConnector {
    room1:: ObjectId,
    room2:: ObjectId,
    roomOffset:: Vector2
} deriving (Show, Generic)

data Grid = Grid {
    tiles:: [[Tile]]
} deriving (Show, Generic)

data Tile = Tile {
    tileType:: TileType,
    entities:: [Entity],
    tileRoomId::ObjectId
} deriving (Show, Generic)

data TileType = Solid | Open deriving (Show, Generic)

data Entity =
    Entity {
        entityId:: ObjectId,
        entityType:: EntityType
    } deriving (Show, Generic)

data EntityType = Enemy| Powerup | Player| Finish deriving (Show, Generic)

newtype ObjectId = ObjectId {
    objectId :: Int
} deriving (Show, Generic)



-- --------------------------------- GridLayer -----------------------------------------
data Level = Level {
  gl_name:: String,
  gl_grid:: Grid
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

compareObjectId::ObjectId->ObjectId->Bool
compareObjectId obj1 obj2 = (objectId obj1) == (objectId obj2)

reverseVector2::Vector2->Vector2
reverseVector2 (x,y) = ((-1) * x, (-1) * y)

positionToVector::Position->Vector2
positionToVector pos = (x pos, y pos)


divideIntCeil::Int->Int->Int
divideIntCeil value divider = ceiling ((toFloat value) / (toFloat divider))
-- ================================== Grid generation ==================================

createRoom::Int->Int->Int->Room
createRoom objId width height = Room nwObjId nwGrid
  where {
    nwGrid = Grid (generateTiles nwObjId width height);
    nwObjId = (ObjectId objId)
}

generateTiles::ObjectId->Int->Int->[[Tile]]
generateTiles _ _ 0 = [[]]
generateTiles _roomId width height = if(height == 1) then [generateTilesRow width] else
    generateTilesRow width : generateTiles _roomId width (height-1)
    where {
        generateTilesRow::Int->[Tile];
        generateTilesRow 0 = [];
        generateTilesRow x = (createTile _roomId Open []): generateTilesRow (x-1)
    }

combineGrids::Grid->Grid->Vector2->Grid
combineGrids r@(Grid {tiles=tilesG1}) grid2 connectionPoint = r {tiles = combinedTiles}
  where {
    combinedTiles = combineTiles tilesG1 (tiles grid2) connectionPoint
}

combineTiles::[[Tile]]->[[Tile]]->Vector2->[[Tile]]
combineTiles tiles1 tiles2 connectionPoint = afterAddingTiles2
  where{
    connectionX = fst connectionPoint;
    connectionY = snd connectionPoint;
    w1 = getGridWidth tiles1;
    h1 = getGridHeight tiles1;
    w2 = getGridWidth tiles2;
    h2 = getGridHeight tiles2;
    totalWidth = if(connectionX >= 0) then (max (connectionX + w2) w1) else (abs connectionX) + w1;
    totalHeight = if(connectionY >= 0) then (max (connectionY + h2) h1) else (abs connectionY) + h1;
    emptyTiles = generateTiles (ObjectId (-1)) totalWidth totalHeight;
    t1StartX = totalWidth - w1 - connectionX;
    t1StartY = totalHeight - h1 - connectionY;

    afterAddingTiles1 = addTiles emptyTiles tiles1 (calcStartPosTile1 connectionX , calcStartPosTile1 connectionY);
    afterAddingTiles2 = addTiles afterAddingTiles1 tiles2 (calcStartPosTile2 connectionX , calcStartPosTile2 connectionY);
    calcStartPosTile1::Int->Int;
    calcStartPosTile1 v = if v >= 0 then 0 else -v;
    calcStartPosTile2::Int->Int;
    calcStartPosTile2 v = if v > 0 then v else 0;
}

addTiles::[[Tile]]->[[Tile]]->Vector2->[[Tile]]
addTiles originalTiles nwTiles (startX, startY) = [[progressTile tile (x,y) | (x, tile) <- zip[0..] row] | (y, row) <- zip[0..] originalTiles]
  where {
    w = getGridWidth nwTiles;
    h = getGridHeight nwTiles;
    progressTile::Tile->Vector2->Tile;
    progressTile tile (curX, curY) = if (curX >= startX && curY >= startY && curX < (startX+ w) && curY < (startY+h))
      then getTile nwTiles (Position (curX-startX) (curY-startY))
      else getTile originalTiles (Position curX curY)
  }


-- NOT NEEDED?? RoomId is created when creating a Room!
{-
setTilesRoomId::ObjectId->[[Tile]]->[[Tile]]
setTilesRoomId nwRoomId allTiles = [[(setRoomId currentTile) | currentTile <- row] | row <- allTiles]
  where {
    setRoomId::Tile->Tile;
    setRoomId tile = Tile (tileType tile) (entities tile) (tileRoomId tile)
} -}


-- ==================================== GET GRID DATA ======================================
getTile::[[Tile]]->Position->Tile -- idea: make faster(?) by using 2x concate and then calc the index with divide/modulo
getTile tiles position = findInTilesPosition allTiles position
    where {
        allTiles = getTilesPosition tiles;
        posX = (x position);
        posY = (y position);

        findInTilesPosition::[(Position, Tile)]->Position->Tile;
        findInTilesPosition (x:[]) findPos = snd x; --NOTE: if position could not be found the last tile is always returned
        findInTilesPosition (x:xs) findPos = if(isPosition (fst x) findPos) then snd x else findInTilesPosition xs findPos
    }

-- Returns the tile relative to originalPos
getTileRelative::Grid->Position->Vector2->Tile
getTileRelative grid originalPos offset = getTile (tiles grid) (getPositionRelative originalPos offset)

getPositionRelative::Position->Vector2->Position
getPositionRelative originalPos offset = Position ((x originalPos) + fst offset) ((y originalPos) + snd offset)


getTilesPosition::[[Tile]]->[(Position, Tile)]
getTilesPosition tiles = concat [[((Position x y), tile)|(x, tile) <- zip[0..] row]|(y, row) <- zip[0..] tiles]

getGridWidth::[[Tile]]->Int
getGridWidth tiles = length (head tiles)

getGridHeight::[[Tile]]->Int
getGridHeight tiles = length tiles


getGridSize::[[Tile]]->Vector2
getGridSize tiles = (getGridWidth tiles, getGridHeight tiles)
{-
getGridWidth::[Grid]->Int
getGridWidth grid = length (head (tiles grid))

getGridHeight::Grid->Int
getGridHeight grid = length (tiles grid)
-}


compareTileTypes::TileType->TileType->Bool;
compareTileTypes Solid Solid = True;
compareTileTypes Open Open = True;
compareTileTypes _ _ = False

isPosition::Position->Position->Bool
isPosition pos1 pos2 = if ((x pos1) == (x pos2) && (y pos1) == (y pos2)) then True else False

{-
isPositionInGrid::Grid->Position->Bool
isPositionInGrid grid pos = True
-}

findRoom::[Room]->ObjectId->Maybe Room
findRoom allRooms currentRoomId = result
  where{
    result = if((length allRoomsFound) > 0) then Just (allRoomsFound!!0) else Nothing;
    allRoomsFound = [r | r <- allRooms, compareObjectId (roomId r) currentRoomId]
}


-- ============================ Smart constructors =========================================
createTile::ObjectId->TileType->[Entity]->Tile
createTile _roomId tileType entitiesTile = Tile tileType entitiesTile _roomId



-- =================================== NOT USED ================================
data Rule = Rule deriving (Show)
