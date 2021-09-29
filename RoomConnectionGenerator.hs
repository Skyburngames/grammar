module RoomConnectionGenerator
(
  connectRoomHorizontal,
  connectRoomVertical,
  openCriticalPathLevel,

  --TESTING
  getCriticalPositionsInRoom,
  getDoorPositionsInRoom
  -- createDoorsLevel
) where

import Grammar
import TileModifiers
import Pathfinder



connectRoomHorizontal::Room->Room->RoomConnector --places room2 on the right of room1
--connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 4 2) (Position 0 2) Horizontal
connectRoomHorizontal room1 room2 = RoomConnector (roomId room1) (roomId room2) (5,0)

connectRoomVertical::Room->Room->RoomConnector -- places room2 below room1
--connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (Position 2 0) (Position 2 4) Vertical
connectRoomVertical room1 room2 = RoomConnector (roomId room1) (roomId room2) (0,5)





-- PATHFINDING --
openCriticalPathLevel::Level->Level
openCriticalPathLevel level = nwLevel --openCriticalPathRoom on each Room
  where{
    nwLevel = Level (name level) (map (openCriticalPathRoom (roomConnections level)) (rooms level)) (roomConnections level);
}


-- PRIVATE
openCriticalPathRoom::[RoomConnector]->Room->Room
openCriticalPathRoom roomConnectors room = Room (roomId room) (Grid nwTiles) --TODO: 1> find all CP in this room, then openCriticalPathTiles from CP-1 to CP-2, CP-1 to CP-3, CP-2 to CP-3 etc
  where{
    criticalPositions = getCriticalPositionsInRoom roomConnectors room; --find all the criticalPositions in this room
    nwTiles = openCriticalPathsBetweenManyToManyPositions (tiles (grid room)) criticalPositions; --open paths so all the criticalPositions are connected

    openCriticalPathsBetweenManyToManyPositions::[[Tile]]->[Position]->[[Tile]]; --open a path between each of the criticalPositions
    openCriticalPathsBetweenManyToManyPositions startTiles [] = startTiles;
    openCriticalPathsBetweenManyToManyPositions startTiles (currentCritPos:otherCritPos) = openCriticalPathsBetweenManyToManyPositions (openCriticalPathsBetweenOneToManyPositions startTiles currentCritPos otherCritPos) otherCritPos
      where {
        openCriticalPathsBetweenOneToManyPositions::[[Tile]]->Position->[Position]->[[Tile]]; -- open a path between the currentPosition and each of the destination positions
        openCriticalPathsBetweenOneToManyPositions _startTiles startPos [] = gb_editTilesOnPath _startTiles [startPos] (setTileType Open); --even if the startPos has no end position (f.e. a empty room) still make a path with just the startPos (this clears tile on the door location) --_startTiles;
        openCriticalPathsBetweenOneToManyPositions _startTiles startPos (currentDestPos: otherDestPositions) =
          openCriticalPathsBetweenOneToManyPositions openCriticalPathFunc startPos otherDestPositions
          where {
            shortestPath = getShortestPath _startTiles startPos currentDestPos; -- find the shortestPath
            openCriticalPathFunc = gb_editTilesOnPath _startTiles shortestPath (setTileType Open) -- set the tileTypes to open on all the tiles on the path
          }

    }
}


getCriticalPositionsInRoom::[RoomConnector]->Room->[Position] --returns a list with all the CriticalPositions in a room (either the player, levelEnd or doors)
getCriticalPositionsInRoom roomConnectors room = otherCriticalPositions++doorPositions
  where{
    allTiles = tiles (grid room);
    otherCriticalPositions = [Position x y | (y, row) <- zip[0..] allTiles, (x, tile) <- zip[0..] row, doEntitiesContainCriticalPoint (entities tile)];
    doorPositions = getDoorPositionsInRoom roomConnectors room;
}


-- return a list with all the doorPositions in a room (based on the roomConnectors)
getDoorPositionsInRoom::[RoomConnector]->Room->[Position] --NOTE: doors are placed in the middle of the room
--getDoorPositionsInRoom [] room = []
getDoorPositionsInRoom roomConnectors room = [getDoorPosition room rc |rc<-(getRoomConnectorsFromRoomPerspective room roomConnectors), isRoomRelevant (room1 rc)]
  where{
    getDoorPosition::Room->RoomConnector->Position;
    -- getDoorPosition _room _rc = Position (if(offsetX == 0) then 0 else calcPositionX) (if(offsetY == 0) then 0 else calcPositionY)
    getDoorPosition _room _rc = Position calcPositionX calcPositionY --TODO: GOES WRONG!!!
      where {
        calcPositionX = if(offsetX == 0) then (divideIntCeil gridWith 2)-1 else (if(offsetX > 0) then (gridWith-1) else 0);
        calcPositionY = if(offsetY == 0) then (divideIntCeil gridHeight 2)-1 else (if(offsetY > 0) then (gridHeight-1) else 0); --if(offsetY > 0) then 0 else (gridHeight-1);
        offsetX = (fst(roomOffset _rc));
        offsetY = (snd(roomOffset _rc));
        gridWith = getGridWidth (tiles (grid _room));
        gridHeight = getGridHeight (tiles (grid _room));
      };

    isRoomRelevant::ObjectId->Bool;
    isRoomRelevant rId = compareObjectId rId ((roomId) room);
    -- test1 = [Position (fst(roomOffset rc)) (snd(roomOffset rc))|rc<-(getRoomConnectorsFromRoomPerspective room roomConnectors)];
    getRoomConnectorsFromRoomPerspective::Room->[RoomConnector]->[RoomConnector];
    getRoomConnectorsFromRoomPerspective currentRoom [] = [];
    getRoomConnectorsFromRoomPerspective currentRoom (curRC:otherRCs) = (progressCurRC):(getRoomConnectorsFromRoomPerspective currentRoom otherRCs)
      where{
        progressCurRC = if(compareObjectId (roomId currentRoom) (room1 curRC)) then curRC else RoomConnector (room2 curRC) (room1 curRC) (((fst (roomOffset curRC)) * (-1)) , ((snd (roomOffset curRC)) * (-1)))
      }
}


-- checks if a list with entities somewhere contains a element that is marked as a criticalPoint
doEntitiesContainCriticalPoint::[Entity]->Bool
doEntitiesContainCriticalPoint [] = False
doEntitiesContainCriticalPoint (currentEntity:otherEntities) = if(isEntityTypeCritical (entityType currentEntity)) then True else doEntitiesContainCriticalPoint otherEntities
  where{
    isEntityTypeCritical::EntityType->Bool;
    isEntityTypeCritical Player = True;
    isEntityTypeCritical Finish = True;
    isEntityTypeCritical _ = False;
}











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
