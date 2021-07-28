module Generator
(
    -- generateTiles,
    -- generateTilesRow,
    -- createGrid,
    -- createSolidSquare,
    -- outlineTiles,
    -- getHighestPosX,
    -- getHighestPosY,
    -- editTileBasedOnPosition,
    -- setTileType,
    -- setTileTypeWall,
    -- applyFuncsToGrid,
    -- editTiles,
    -- setTileType,
    -- addEntity,
    -- mapTile,
    --mapTileWithPosition,
    -- setTileTypeInPosition,
    --editTile,
    --editBorderTile
) where

import Grammar
import System.Random


{-
data TileWithPosition = TileWithPosition {
    tile:: Tile,
    x:: Int,
    y:: Int
}
-}


-- type TileModifier =






-- ========================================== MODIFY GRID ====================================================
{-
mapTile::(Tile->Tile)->[[Tile]]->[[Tile]]
mapTile tileFunction allTiles = (map.map) tileFunction allTiles


setTileTypeInPosition::Position->TileType->Position->Tile->Tile
setTileTypeInPosition requiredPosition tileType position tile = 
    if (isPosition requiredPosition position) then (setTileType tileType tile)
    else tile


editTile::Position->(Tile->Tile)->Position->Tile->Tile
editTile requiredPosition tileFunc position tile = if (isPosition requiredPosition position) then (tileFunc tile)
    else tile

editBorderTile::Int->Int->(Tile->Tile)->Position->Tile->Tile
editBorderTile w h tileFunc position tile = if (posX == 0 || posX == (w-1) || posY == 0 || posY == (h-1)) then (tileFunc tile) else tile
    where {
        posX = (Grammar.x position); 
        posY = (Grammar.y position)
        }
-}

