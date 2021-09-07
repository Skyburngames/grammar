module RoomConnectionGenerator
(
  connectRoomHorizontal,
  connectRoomVertical,
  createDoorsLevel
) where

import Grammar
import TileModifiers

connectRoomHorizontal::Room->Room->RoomConnector --places room2 on the right of room1
connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 4 2) (Position 0 2) Horizontal

connectRoomVertical::Room->Room->RoomConnector -- places room2 below room1
connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 2 0) (Position 2 4) Vertical


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
      -- tilesAfterDoorsR2 = (createDoors tilesAfterDoorsR1 (getRoomConnectionsRoom room roomConnections))
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


    {-
    createDoors2::[[Tile]]->[RoomConnector]->[[Tile]]
    createDoors2 tiles roomConnections = nwTiles
      where{
        nwTiles = [[progressTile tile (Position x y) | (x, tile) <- zip[0..] row] | (y, row) <- zip[0..] tiles];
        progressTile tile pos = if(isPositionDoor pos roomConnections)
          then (setTileType Open tile)
          else tile;
    }
    -}



{-
isPositionDoor::Position->[RoomConnector]->Bool
isPositionDoor pos [] = False
isPositionDoor pos (rc:roomConnectors) =
  if((isPosition pos (r1ConnectionPoint rc)) || (isPosition pos (r2ConnectionPoint rc))) then True
    else isPositionDoor pos roomConnectors -}

{-
getRoomConnectionsRoom2::ObjectId->[RoomConnector]->[RoomConnector]
getRoomConnectionsRoom2 roomId roomConnections = [rc|rc<-roomConnections, objectIdInRoomConnector rc roomId]
  where{
    objectIdInRoomConnector::RoomConnector->ObjectId->Bool;
    objectIdInRoomConnector roomConnector _objectId =
      ((objectId _objectId) == (objectId (room1 roomConnector))) || ((objectId _objectId) == (objectId (room2 roomConnector)))
}
-}
