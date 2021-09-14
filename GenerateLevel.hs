{-
  This Module represents the Gridlayer. Use this module to combine Level->Room->(RoomConnector | Tile) to GeneratedLevel->Tile
-}

module GenerateLevel
(
  progressLevel,
  getConnectedRooms,
  getConnectedRoomIds
) where

import Grammar


progressLevel::Level->GeneratedLevel
progressLevel level = generateLevel emptyGeneratedLevel level (rooms level)
  where {
    emptyGeneratedLevel = GeneratedLevel (name level) (Grid [])
}
--generateLevel level = GeneratedLevel (name level) (grid ((rooms level)!!0))


generateLevel::GeneratedLevel->Level->[Room]->GeneratedLevel
generateLevel gLevel _ [] = gLevel
generateLevel gLevel level (roomTodo: otherRoomsTodo) =
  if(doesRoomExcist gLevel (roomId roomTodo)) then generateLevel gLevel level nwRoomsTodoRemaining
    else generateLevel nwGeneratedLevel level nwRoomsTodoRemaining
  where {
    nwGeneratedLevel = generateRoomInGeneratedLevel gLevel roomTodo (roomConnections level); -- generate the room
    -- nwGeneratedLevel = generateAllConnectedRooms gLevel roomTodo (roomConnections level);
    -- nwRoomsTodoRemaining = connectedRoomsNotYetMade ++ otherRoomsTodo; --
    nwRoomsTodoRemaining = [ r|r<-allTodoRooms, not (doesRoomExcist nwGeneratedLevel (roomId r))]; -- filter on rooms that arent made yet
    allTodoRooms = connectedRooms++otherRoomsTodo; --combine the connected rooms with the rooms that where already todo
    connectedRooms = (getConnectedRooms level (roomId roomTodo)); -- all rooms connected to roomTodo
}


-- addToFrontIfNotAlreadyInList::[Room]->[Room]->[Room]
-- addIfNotAlreadyInList roomsA roomsB = roomsA ++ roomsB --TODO

getConnectedRooms::Level->ObjectId->[Room]
getConnectedRooms level roomId = result
  where {
    allRooms = rooms level;
    result = [rm | Just rm <- resultMaybe];
    resultMaybe = [findRoom allRooms cr_id| cr_id <- combinedRoomIds];
    combinedRoomIds = toRC++fromRC;
    toRC = [(room2 rc) |rc<-releventRC, (compareObjectId (room1 rc) roomId)];
    fromRC = [(room1 rc) |rc<-releventRC, (compareObjectId (room2 rc) roomId)];
    releventRC = [ rc |rc<-(roomConnections level), (compareObjectId (room1 rc) roomId) || (compareObjectId (room2 rc) roomId)];
}

getConnectedRoomIds::Level->ObjectId->[ObjectId]
getConnectedRoomIds level cur_roomId = result
  where {
    allRooms = (getConnectedRooms level cur_roomId);
    result = [(roomId r) |r <- allRooms]
  }

{-
getTileData::[[Tile]]->Position->Vector2->[[Tile]] --TODO
getTileData allTiles startPos size = [[]]
  where{
    result = [[(tile, Position x y) | (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId] | (y, row) <- zip[0..] allTiles];
}-}


generateRoomInGeneratedLevel::GeneratedLevel->Room->[RoomConnector]->GeneratedLevel --make this room but make sure it is located on the valid position based on roomConnectors and the current generatedLevel
generateRoomInGeneratedLevel gLevel room roomConnectors =
  --check if the room already excist in gLevel
  if (doesRoomExcist gLevel (roomId room))
    then gLevel
    else addRoomToGeneratedLevel gLevel roomConnectors room

--generateRoomAndAllConnectedRooms::GeneratedLevel->Room->[RoomConnector]->GeneratedLevel -- check if connected room already excist
--generateRoomAndAllConnectedRooms gLevel room roomConnectors = gLevel --TODO



getOriginWorldPosition::GeneratedLevel->ObjectId->(Maybe Position) --looks like getTileData??
getOriginWorldPosition gLevel roomId = firstPosFromRoomId --Just (Position 0 0) --TODO
  where{
    allTiles = tiles (gl_grid gLevel);
    firstPosFromRoomId = if((length roomTilesFromRoomId) > 0) then Just (snd ((roomTilesFromRoomId!!0)!!0)) else Nothing; --get the first (tile,position) from the tiles that belong to roomId and return the position OR Nothing
    roomTilesFromRoomId = getTilesWithWorldPosition allTiles roomId; --[[(tile, Position x y) | (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId] | (y, row) <- zip[0..] allTiles];
  }


getTilesWithWorldPosition::[[Tile]]->ObjectId->[[(Tile, Position)]]
getTilesWithWorldPosition allTiles roomId = [[(tile, Position x y) | (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId] | (y, row) <- zip[0..] allTiles];

doesRoomExcist::GeneratedLevel->ObjectId->Bool --looks like: getOriginWorldPosition??
doesRoomExcist gLevel roomId = case (getOriginWorldPosition gLevel roomId) of
  Just val -> True
  Nothing -> False





addRoomToGeneratedLevel::GeneratedLevel->[RoomConnector]->Room->GeneratedLevel
addRoomToGeneratedLevel gLevel roomConnectors room =
  if (doesRoomExcist gLevel (roomId room))
    then gLevel --does already excist
  else
    --check there are RC that link to the nwRoom and to an room that already excist in the gLevel
    if ((length rcLinkingToExcistingRoom) <= 0)
      then GeneratedLevel (gl_name gLevel) (grid room) -- create a new gLevel with only this room in it
      else addRoom rcToUse
    where {
      rcToUse = rcLinkingToExcistingRoom!!0;
      rcLinkingToExcistingRoom = [ rc | rc <-rcWithCurrentRoom, (doesRoomExcist gLevel (room1 rc)) || (doesRoomExcist gLevel (room2 rc))];
      rcWithCurrentRoom = [ rc|rc<-roomConnectors, ((objectId (room1 rc)) == (objectId (roomId room)) || (objectId (room2 rc)) == (objectId (roomId room)))];
      addRoom::RoomConnector->GeneratedLevel;
      addRoom rc = if (compareObjectId (room2 rc) (roomId room))
        then addGridToGeneratedLevel gLevel room (room1 rc) (roomOffset rc)  --rc.room1 == already in gLevel and rc.room2 == room
        else addGridToGeneratedLevel gLevel room (room2 rc) (reverseVector2 (roomOffset rc))--REVERSE: rc.room2 == already in gLevel and rc.room1 == room, use reverse on rc.roomOffset
    }

addGridToGeneratedLevel::GeneratedLevel->Room->ObjectId->Vector2->GeneratedLevel
addGridToGeneratedLevel gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId; --getTileData generatedLevelAllTiles startPos roomSize;
    tilesExcistingRoom = [[fst posTile | (x, posTile) <- zip[0..] row] | (y, row) <- zip[0..] tilePosExcistingRoom];
    tilesNwRoom = tiles (grid nwRoom);
    startPosMaybe = getOriginWorldPosition gLevel (roomId nwRoom);
    startPos = (case startPosMaybe of
      Nothing  -> Position 0 0 --use 0 0 if startPos is not found (should never happen)
      Just val -> val);
    roomSize = getGridSize (tiles (grid nwRoom));
    combinedRoomTiles = combineTiles tilesExcistingRoom tilesNwRoom offset;
    resultTiles = combineTiles generatedLevelAllTiles combinedRoomTiles (positionToVector startPos);
    resultGeneratedLevel = GeneratedLevel (gl_name gLevel) (Grid resultTiles);
}
