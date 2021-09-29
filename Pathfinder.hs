module Pathfinder
(
  gb_editTilesOnPath,
  getShortestPath
) where

import Grammar



gb_editTilesOnPath::[[Tile]]->[Position]->TileModifier->[[Tile]]
gb_editTilesOnPath tiles [] tileModFunc = tiles
gb_editTilesOnPath tiles (currentPathPos:otherPathPos) tileModFunc = gb_editTilesOnPath nwTiles otherPathPos tileModFunc
  where{
    nwTiles = [[progressTile tile (Position tileX tileY) | (tileX, tile) <- zip[0..] row] | (tileY, row) <- zip[0..] tiles];
    progressTile::Tile->Position->Tile;
    progressTile tile currentPos = if(isPosition currentPos currentPathPos) then (tileModFunc tile) else tile;
  }



getShortestPath::[[Tile]]->Position->Position->[Position]
getShortestPath tiles startPos endPos = createPath [] startPos --uses getDistancesFromEndPosition and getShortestPathFromDistanceData
  where{
    endPosX = (x endPos);
    endPosY = (y endPos);
    createPath::[Position]->Position->[Position];
    createPath currentPath currentPos = if(isPosition currentPos endPos) then currentPath else
      createPath (nwCurrentPos:currentPath) nwCurrentPos

      --if(not(deltaX == 0))then moveOnX else
      --if(not(deltaY == 0)) then moveOnY else
    where {
      deltaX = endPosX - currentPosX;
      deltaY = endPosY - currentPosY;
      currentPosX = (x currentPos);
      currentPosY = (y currentPos);
      nwCurrentPos = if(not(deltaX == 0))then moveOnX else moveOnY; --only call this if destination is not reached!

      moveOnX = if(deltaX > 0) then Position (currentPosX+1) currentPosY else Position (currentPosX-1) currentPosY;
      moveOnY = if(deltaY > 0) then Position currentPosX (currentPosY+1) else Position currentPosX (currentPosY-1);
  }

}


-- ================================= PRIVATE =========================================
{-
getDistancesFromEndPosition::[[Tile]]->Position->Position->[[(Tile, Position, Int)]]
getDistancesFromEndPosition tiles startPos endPos = [[]]

getShortestPathFromDistanceData::[[(Tile, Position, Int)]]->[Position]
getShortestPathFromDistanceData tilesWithDistanceData = []
-}
