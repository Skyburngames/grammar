module RoomConnectionGenerator
(
  connectRoomHorizontal,
  connectRoomVertical,
  openCriticalPath
  -- createDoorsLevel
) where

import Grammar
import TileModifiers



connectRoomHorizontal::Room->Room->RoomConnector --places room2 on the right of room1
--connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 4 2) (Position 0 2) Horizontal
connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (5,0)

connectRoomVertical::Room->Room->RoomConnector -- places room2 below room1
--connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 2 0) (Position 2 4) Vertical
connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (0,5)

-- PATHFINDING --

openCriticalPath::[[Tile]]->Position->Position->[[Tile]]
openCriticalPath tiles startPos endPos = tiles



-- PRIVATE
-- getCriticalPositions::

getDistancesFromEndPosition::[[Tile]]->Position->Position->[[(Tile, Position, Int)]]
getDistancesFromEndPosition tiles startPos endPos = [[]]

getShortestPath::[[(Tile, Position, Int)]]->[Position]
getShortestPath tilesWithDistanceData = []

gb_editTilesOnPath::[[Tile]]->[Position]->TileModifier->[[Tile]]
gb_editTilesOnPath tiles path tileModFunc = tiles









-- ========================================================== OLD ===================================================================
{-
createDoorsLevel::Level->Level
createDoorsLevel level = Level (name level) (nwRooms (rooms level)) _roomConnections
  where{
    _roomConnections = (roomConnections level);
    nwRooms::[Room]->[Room];
    nwRooms rooms = [createDoorsRoom r _roomConnections|r<- rooms]
}
-- ======================================== PRIVATE ==============================================

createDoorsRoom::Room->[RoomConnector]->Room
createDoorsRoom room roomConnections = Room (roomId room) (nwGrid (grid room))
  where{
    nwGrid::Grid->Grid;
    nwGrid _grid = Grid tilesAfterDoorsR1
    where{
      tilesAfterDoorsR1 = (createDoors (tiles _grid) (getDoorPositions room roomConnections));
  }
}

createDoors::[[Tile]]->[Position]->[[Tile]]
createDoors tiles doorPositions = nwTiles
  where{
    nwTiles = [[progressTile tile (Position x y) | (x, tile) <- zip[0..] row] | (y, row) <- zip[0..] tiles];
    progressTile tile pos = if(isPositionInList pos doorPositions)
      then (setTileType Open tile)
      else tile;
}

getDoorPositions::Room->[RoomConnector]->[Position]
getDoorPositions room roomConnections = [(getConnectionPoint rc)|rc<-roomConnections, objectIdInRoomConnector rc _roomId]
  where{
    _roomId = (roomId room);
    roomNr = (objectId _roomId);
    objectIdInRoomConnector::RoomConnector->ObjectId->Bool;
    objectIdInRoomConnector roomConnector _objectId =
      ((objectId _objectId) == (objectId (room1 roomConnector))) || ((objectId _objectId) == (objectId (room2 roomConnector)));
    getConnectionPoint::RoomConnector->Position; --make sure roomNr ==either room1 or room2
    getConnectionPoint rc = if((roomNr == (objectId (room1 rc)))) then (r1ConnectionPoint rc) else (r2ConnectionPoint rc)
}

isPositionInList::Position->[Position]->Bool
isPositionInList pos [] = False
isPositionInList pos (cur_pos:otherPos) =
  if((isPosition pos cur_pos)) then True
    else isPositionInList pos otherPos
-}
