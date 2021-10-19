module Pathfinder
(
  gb_editTilesOnPath,
  getShortestPath
) where

import Grammar


-- use this function to edit tiles on a path
gb_editTilesOnPath::[[Tile]]->[Position]->TileModifier->[[Tile]]
gb_editTilesOnPath tiles [] tileModFunc = tiles
gb_editTilesOnPath tiles (currentPathPos:otherPathPos) tileModFunc = gb_editTilesOnPath nwTiles otherPathPos tileModFunc
  where{
    nwTiles = [[progressTile tile (Position tileX tileY) | (tileX, tile) <- zip[0..] row] | (tileY, row) <- zip[0..] tiles];
    progressTile::Tile->Position->Tile;
    progressTile tile currentPos = if(isPosition currentPos currentPathPos) then (tileModFunc tile) else tile;
  }


-- this function returns the shortest path between start and end
getShortestPath::[[Tile]]->Position->Position->[Position]
getShortestPath tiles startPos endPos = createPath [startPos] startPos
  where{
    endPosX = (x endPos);
    endPosY = (y endPos);
     --keep adding something to the path until currentPos is endPos
    createPath::[Position]->Position->[Position];
    createPath currentPath currentPos = if(isPosition currentPos endPos) then currentPath else
      createPath (nwCurrentPos:currentPath) nwCurrentPos
    where {
      deltaX = endPosX - currentPosX;
      deltaY = endPosY - currentPosY;
      currentPosX = (x currentPos);
      currentPosY = (y currentPos);
      --only call this if destination is not reached!
      nwCurrentPos = if(not(deltaX == 0))then moveOnX else moveOnY;

      moveOnX = if(deltaX > 0) then Position (currentPosX+1) currentPosY else Position (currentPosX-1) currentPosY;
      moveOnY = if(deltaY > 0) then Position currentPosX (currentPosY+1) else Position currentPosX (currentPosY-1);
  }
}
