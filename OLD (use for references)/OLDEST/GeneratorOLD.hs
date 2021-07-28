module Generator
(
    generateTiles,
    -- generateTilesRow,
    -- createGrid,
    -- createSolidSquare,
    -- outlineTiles,
    -- getHighestPosX,
    -- getHighestPosY,
    -- editTileBasedOnPosition,
    -- setTileType,
    -- setTileTypeWall,
    applyFuncsToGrid,
    editTiles,
    setTileType,
    addEntity,
    mapTile,
    mapTileWithPosition,
    setTileTypeInPosition,
    editTile
) where

import Grammar




{-
applyFuncsToGrid::[([Tile]->[Tile])]->[Tile]->[Tile]
applyFuncsToGrid [] tiles = tiles
applyFuncsToGrid (currentFunction: otherFunctions) tiles = applyFuncsToGrid otherFunctions (currentFunction tiles)
-}

applyFuncsToGrid::[([[Tile]]->[[Tile]])]->[[Tile]]->[[Tile]]
applyFuncsToGrid [] startTiles = startTiles
applyFuncsToGrid (currentFunction: otherFunctions) startTiles = applyFuncsToGrid otherFunctions (currentFunction startTiles)


data TileWithPosition = TileWithPosition {
    tile:: Tile,
    x:: Int,
    y:: Int
}


-- ================================== Grid generation ================================== 
{-
createGrid::Int->Int->Grid
createGrid width height = Grid (generateTiles width height)
-}

generateTiles::Int->Int->[[Tile]]
generateTiles width 0 = [[]]
generateTiles width height = generateTilesRow width : generateTiles width (height-1)

generateTilesRow::Int->[Tile]
generateTilesRow 0 = []
generateTilesRow x = (createTile Open []): generateTilesRow (x-1)

-- ========================================== Version 3 ====================================================
mapTile::(Tile->Tile)->[[Tile]]->[[Tile]]
mapTile tileFunction allTiles = (map.map) tileFunction allTiles


mapTileWithPosition::(Position->Tile->Tile)->[[Tile]]->[[Tile]]
mapTileWithPosition tilePosFunction allTiles = [[tilePosFunction (Position x y) tile|(x, tile) <- zip[0..] row]|(y, row) <- zip[0..] allTiles]


setTileTypeInPosition::Position->TileType->Position->Tile->Tile
setTileTypeInPosition requiredPosition tileType position tile = 
    if (isPosition requiredPosition position) then (setTileType tileType tile)
    else tile


editTile::Position->(Tile->Tile)->Position->Tile->Tile
editTile requiredPosition tileFunc position tile = if (isPosition requiredPosition position) then (tileFunc tile)
    else tile


-- ========================================== Version 2 ====================================================
editTiles::Int->Int->(Tile->Tile)->[[Tile]]->[[Tile]]
editTiles reqX reqY tileFunc [] = [[]]
editTiles reqX reqY tileFunc rows = editTiles' rows reqX reqY tileFunc 0
    where {
        editTiles' [[]] _ _ _ _ = [[]];
        editTiles' (currentRow:otherRows) reqX reqY tileFunc y = (editRow currentRow reqX reqY tileFunc y): (editTiles' otherRows reqX reqY tileFunc (y+1));

    }


editRow::[Tile]->Int->Int->(Tile->Tile)->Int->[Tile]
editRow tiles reqX reqY tileFunc posY = editRow' tiles reqX reqY tileFunc 0 posY
    where {
        editRow' [] _ _ _ _ _ = [];
        editRow' (currentTile:otherTiles) reqX reqY tileFunc x y =
            if (reqX==x && reqY==y) then (tileFunc currentTile): (editRow' otherTiles reqX reqY tileFunc (x+1) y)
            else currentTile: (editRow' otherTiles reqX reqY tileFunc (x+1) y)
    }


setTileType::TileType->(Tile->Tile)
setTileType nwTileType = \r@(Tile {tileType = tp}) -> r {tileType=nwTileType}

addEntity::Entity->(Tile->Tile)
addEntity nwEntity = \r@(Tile {entities = ent}) -> r {entities=nwEntity:ent}


{-
editTiles::[[Tile]]->(Int->Int->Bool)->(Tile->Tile)->[[Tile]]
editTiles [] condition tileFunc = [[]]
editTiles rows condition tileFunc = editTiles' rows condition tileFunc 0
    where {
        editTiles' [[]] _ _ _ = [[]];
        editTiles' (currentRow:otherRows) condition tileFunc y = (editRow currentRow condition tileFunc y): (editTiles' otherRows condition tileFunc (y+1));

    }


editRow::[Tile]->(Int->Int->Bool)->(Tile->Tile)->Int->[Tile]
editRow tiles condition tileFunc posY = editRow' tiles condition tileFunc 0 posY
    where {
        editRow' [] _ _ _ _ = [];
        editRow' (currentTile:otherTiles) condition tileFunc x y =
            if (condition x y) then (tileFunc currentTile): (editRow' otherTiles condition tileFunc (x+1) y)
            else currentTile: (editRow' otherTiles condition tileFunc (x+1) y)
    }



-}



{-
-- ========================================== Version 1 ====================================================
editTileBasedOnPosition::[[Tile]]->(TileWithPosition->Bool)->(Tile->Tile)->[[Tile]]
editTileBasedOnPosition allTiles condition tileFunc = transformTileWithPosTo2D (editTileBasedOnPosition' (transform2DToTileWithPos allTiles) condition tileFunc)

editTileBasedOnPosition'::[TileWithPosition]->(TileWithPosition->Bool)->(Tile->Tile)->[TileWithPosition]
editTileBasedOnPosition' [] condition tileFunc = []
editTileBasedOnPosition' (currentTileWPos: otherTilesWPos) condition tileFunc = 
    if (condition currentTileWPos) 
        then (TileWithPosition (tileFunc (tile currentTileWPos)) (x currentTileWPos) (y currentTileWPos)): (editTileBasedOnPosition' otherTilesWPos condition tileFunc)
        else currentTileWPos:(editTileBasedOnPosition' otherTilesWPos condition tileFunc)


-- ------------------- HELPER functions ------------------- 
transform2DToTileWithPos:: [[Tile]]->[TileWithPosition]
transform2DToTileWithPos tiles =  [(TileWithPosition tile x y) | (x,row) <- zip[0..] tiles, (y,tile) <- zip[0..] row]

-- transformRowToTileWithPos:: [Tile]->Int->[TileWithPosition]
-- transformRowToTileWithPos allTiles posY = [(TileWithPosition currentTile 0 posY)|currentTile <- allTiles]
-- transformRowToTileWithPos rows posY = [(TileWithPosition tile x y) | (x,row) <- zip[0..] rows, (y,tile) <- zip[0..] row]

transformTileWithPosTo2D:: [TileWithPosition] -> [[Tile]]
transformTileWithPosTo2D (currentTileWPos: otherTilesWPos) = [[]] --TODO


setTileType::TileType->(Tile->Tile)
setTileType nwTileType = \r@(Tile {tileType = tp}) -> r {tileType=nwTileType}

addEntity::Entity->(Tile->Tile)
addEntity nwEntity = \r@(Tile {entities = ent}) -> r {entities=nwEntity:ent}


setTileTypeWall::Tile->Tile 
setTileTypeWall tile = setTileType Solid tile
-- ==================================================================================================================
-}







{-
editTile::(Tile->Bool)->(Tile->Tile)->[[Tile]]->[[Tile]]
editTile _ _ [] = []
--editTile condition tileFunc allTiles = 



editTile'::(Tile->[[Tile]]->Bool)->(Tile->Tile)->[Tile]->[[Tile]]
editTile' _ _ [] = []
editTile' condition tileFunc (currentTile: otherTiles) = 
    if (condition currentTile (currentTile:otherTiles)) then (tileFunc currentTile): (editTile' condition tileFunc otherTiles)
    else currentTile: (editTile' condition tileFunc otherTiles)
-}

{-
editTile condition tileFunc (currentRow:otherRows) = 
    if (condition currentRow) 
        then (tileFunc currentRow): editTile condition tileFunc otherRows
        else editTile condition tileFunc otherRows
-}


-- isInPosition::Tile->[[Tile]]->Int->Int->Bool
-- isInPosition tile allTiles posX posY = True -- TODO









-- ============================================== HELPER functions ===============================================================



-- Create a empty field of width x height (example of list comprehensions)
-- generateTiles::Int->Int->[Tile]->[(Int, Int)]
-- generateTiles width height _  = [(x,y)| x <- [0..width], y <- [0..height]]


{-
-- Set the tileType to solid of a square within a grid
createSolidSquare::Int->Int->Int->Int->[Tile]->[Tile]
createSolidSquare x y w h []= []
createSolidSquare x y w h (tile:otherTiles)=
    if tileInRectangle' (tilePosX tile) (tilePosY tile) x y w h then (setTileType Solid tile) : createSolidSquare x y w h otherTiles
    else tile : createSolidSquare x y w h otherTiles



outlineTiles::[Tile]->[Tile]
outlineTiles [] = []
outlineTiles tiles =
    createSolidSquare 0 (getHighestPosY tiles) (getHighestPosX tiles+1) 1 (createSolidSquare (getHighestPosX tiles) 0 1 (getHighestPosY tiles+1) (createSolidSquare 0 0 1 (getHighestPosY tiles+1) (createSolidSquare 0 0 (getHighestPosX tiles+1) 1 tiles)))





editTile::(Tile->Tile)->GridPosition->[Tile]->[Tile]
editTile _ _ [] = []
editTile tileFunc pos (tile:otherTiles)=
    if tileCompareGridPosition' tile pos then tileFunc tile: otherTiles -- dont continue searching
    else tile: editTile tileFunc pos otherTiles


-- Returns the element in the list with the highest posX
getHighestPosX::[Tile]->Int
getHighestPosX grid = getHighestPosX' grid 0


-- Returns the element in the list with the highest posY
getHighestPosY::[Tile]->Int
getHighestPosY grid = getHighestPosY' grid 0


setTileType::TileType->(Tile->Tile)
setTileType nwTileType = \r@(Tile {tileType = tp}) -> r {tileType=nwTileType}

addEntity::Entity->(Tile->Tile)
addEntity nwEntity = \r@(Tile {entities = ent}) -> r {entities=nwEntity:ent}


setTileTypeWall::Tile->Tile 
setTileTypeWall tile = setTileType Solid tile
 -- ============================================= PRIVATE ====================================================


tileCompareGridPosition'::Tile->GridPosition->Bool
tileCompareGridPosition' tile gridPosition = gridPositionCompareGridPosition' gridPosition (tileGridPosition tile)

gridPositionCompareGridPosition'::GridPosition->GridPosition->Bool 
gridPositionCompareGridPosition' pos1 pos2 = x pos1 == x pos2 && y pos1 == y pos2

tileInRectangle'::Int->Int->Int->Int->Int->Int->Bool
tileInRectangle' posX posY rectX rectY w h =
    if (posX >= rectX && posX < (rectX+w)) && (posY >= rectY && posY < (rectY+h)) then True
    else False


getHighestPosX'::[Tile]->Int->Int
getHighestPosX' [] currentMax = currentMax
getHighestPosX' (tile:otherTiles) currentMax =
    if tilePosX tile > currentMax then getHighestPosX' otherTiles (tilePosX tile)
    else getHighestPosX' otherTiles currentMax

getHighestPosY'::[Tile]->Int->Int
getHighestPosY' [] currentMax = currentMax
getHighestPosY' (tile:otherTiles) currentMax =
    if tilePosY tile > currentMax then getHighestPosY' otherTiles (tilePosY tile)
    else getHighestPosY' otherTiles currentMax



-}