{-
  This Module represents the Gridlayer. Use this module to combine Level->Room->(RoomConnector | Tile) to GeneratedLevel->Tile
-}

module GenerateLevel
(
  progressRoomedLevel,
  getConnectedRooms,
  getConnectedRoomIds,
  getOriginWorldPosition,
  getTilesWithWorldPosition,
  addRoomToLevel,
  addGridToLevel,
  doesRoomExist,
) where

import Grammar


progressRoomedLevel::RoomedLevel->Level
progressRoomedLevel roomedLevel = generateLevel emptyGeneratedLevel roomedLevel (rooms roomedLevel)
  where {
    emptyGeneratedLevel = Level (name roomedLevel) (Grid [])
}


getConnectedRooms::RoomedLevel->ObjectId->[Room]
getConnectedRooms roomedLevel roomId = result
  where {
    allRooms = rooms roomedLevel;
    result = [rm | Just rm <- resultMaybe];
    resultMaybe = [findRoom allRooms cr_id| cr_id <- combinedRoomIds];
    combinedRoomIds = toRC++fromRC;
    toRC = [(room2 rc) |rc<-releventRC, (compareObjectId (room1 rc) roomId)];
    fromRC = [(room1 rc) |rc<-releventRC, (compareObjectId (room2 rc) roomId)];
    releventRC = [ rc |rc<-(roomConnections roomedLevel), (compareObjectId (room1 rc) roomId) || (compareObjectId (room2 rc) roomId)];
}


getConnectedRoomIds::RoomedLevel->ObjectId->[ObjectId]
getConnectedRoomIds roomedLevel cur_roomId = result
  where {
    allRooms = (getConnectedRooms roomedLevel cur_roomId);
    result = [(roomId r) |r <- allRooms]
}


getOriginWorldPosition::Level->ObjectId->(Maybe Position)
getOriginWorldPosition gLevel roomId = firstPosFromRoomId
  where{
    allTiles = tiles (gl_grid gLevel);
    firstPosFromRoomId = if((length roomTilesFromRoomId) > 0 && (length (roomTilesFromRoomId!!0)) > 0) then Just (snd ((roomTilesFromRoomId!!0)!!0)) else Nothing; --get the first (tile,position) from the tiles that belong to roomId and return the position OR Nothing
    roomTilesFromRoomId = getTilesWithWorldPosition allTiles roomId; --[[(tile, Position x y) | (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId] | (y, row) <- zip[0..] allTiles];
  }


getTilesWithWorldPosition::[[Tile]]->ObjectId->[[(Tile, Position)]]
-- getTilesWithWorldPosition allTiles roomId = [[(tile, Position x y) ] |  (y, row) <- zip[0..] allTiles, (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId]
getTilesWithWorldPosition allTiles roomId = filterEmptyRows
  where {
    filterEmptyRows = [ row |row<-tilesWithCorrectRoomId, (length row) > 0];
    tilesWithCorrectRoomId = [[(tile, Position x y) | (x, tile) <- zip[0..] row, compareObjectId (tileRoomId tile) roomId] | (y, row) <- zip[0..] allTiles ];
  }


addRoomToLevel::Level->[RoomConnector]->Room->Level
addRoomToLevel gLevel roomConnectors room =
  if (doesRoomExist gLevel (roomId room))
    then gLevel --does already excist
  else
    --check there are RC that link to the nwRoom and to an room that already excist in the gLevel
    if ((length rcLinkingToExcistingRoom) <= 0)
      then Level (gl_name gLevel) (grid room) -- create a new gLevel with only this room in it
      else addRoom rcToUse
    where {
      rcToUse = rcLinkingToExcistingRoom!!0;
      rcLinkingToExcistingRoom = [ rc | rc <-rcWithCurrentRoom, (doesRoomExist gLevel (room1 rc)) || (doesRoomExist gLevel (room2 rc))];
      rcWithCurrentRoom = [ rc|rc<-roomConnectors, ((objectId (room1 rc)) == (objectId (roomId room)) || (objectId (room2 rc)) == (objectId (roomId room)))];
      addRoom::RoomConnector->Level;
      addRoom rc = if (compareObjectId (room2 rc) (roomId room))
        then addGridToLevel gLevel room (room1 rc) (roomOffset rc)  --rc.room1 == already in gLevel and rc.room2 == room
        else addGridToLevel gLevel room (room2 rc) (reverseVector2 (roomOffset rc))--REVERSE: rc.room2 == already in gLevel and rc.room1 == room, use reverse on rc.roomOffset
    }


addGridToLevel::Level->Room->ObjectId->Vector2->Level
addGridToLevel gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    roomSize = getGridSize (tiles (grid nwRoom));

    -- crop the room from the generatedLevel
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId;
    tilesExcistingRoom = [[tile | (tile, pos) <- row] | (row) <- tilePosExcistingRoom];

    tilesNwRoom = tiles (grid nwRoom);

    -- find the start position of the relativeToRoomId in the generatedLevel
    startPosMaybe = getOriginWorldPosition gLevel relativeToRoomId;
    startPos = (case startPosMaybe of
      Nothing  -> Position 0 0 --use 0 0 if startPos is not found (should never happen)
      Just val -> val);

    -- combines
    combinedRoomTiles = combineTiles tilesExcistingRoom tilesNwRoom offset; --first combine the cropped old room-tiles with the new room-tiles
    finalStartPos = ( if(fst offset)>=0 then (x startPos) else (fst offset),
                      if(snd offset)>=0 then (y startPos) else (snd offset)); --based on offset calc the startPos for the combined room
    resultTiles = combineTiles generatedLevelAllTiles combinedRoomTiles finalStartPos; --second combine the combined room-tiles in the generatedLevel-tiles
    resultGeneratedLevel = Level (gl_name gLevel) (Grid resultTiles);
}

doesRoomExist::Level->ObjectId->Bool
doesRoomExist gLevel roomId = case (getOriginWorldPosition gLevel roomId) of
  Just val -> True
  Nothing -> False



-- ------------------------------------------- PRIVATE --------------------------------------------------
generateLevel::Level->RoomedLevel->[Room]->Level
generateLevel gLevel _ [] = gLevel
generateLevel gLevel roomedLevel (roomTodo: otherRoomsTodo) =
  if(doesRoomExist gLevel (roomId roomTodo)) then generateLevel gLevel roomedLevel nwRoomsTodoRemaining
    else generateLevel nwGeneratedLevel roomedLevel nwRoomsTodoRemaining
  where {
    nwGeneratedLevel = generateRoomInLevel gLevel roomTodo (roomConnections roomedLevel); -- generate the room
    nwRoomsTodoRemaining = [ r|r<-allTodoRooms, not (doesRoomExist nwGeneratedLevel (roomId r))]; -- filter on rooms that arent made yet
    allTodoRooms = connectedRooms++otherRoomsTodo; --combine the connected rooms with the rooms that where already todo
    connectedRooms = (getConnectedRooms roomedLevel (roomId roomTodo)); -- all rooms connected to roomTodo
}

generateRoomInLevel::Level->Room->[RoomConnector]->Level --make this room but make sure it is located on the valid position based on roomConnectors and the current generatedLevel
generateRoomInLevel gLevel room roomConnectors =
  --check if the room already excist in gLevel
  if (doesRoomExist gLevel (roomId room))
    then gLevel
    else addRoomToLevel gLevel roomConnectors room










-- ======================================== OLD ==============================================
{-
addGridToLevel::GeneratedLevel->Room->ObjectId->Vector2->GeneratedLevel
addGridToLevel gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId; --getTileData generatedLevelAllTiles startPos roomSize;
    tilesExcistingRoom = [[fst posTile | (x, posTile) <- zip[0..] row] | (y, row) <- zip[0..] tilePosExcistingRoom];
    tilesNwRoom = tiles (grid nwRoom);
    startPosMaybe = getOriginWorldPosition gLevel relativeToRoomId;
    startPos = (case startPosMaybe of
      Nothing  -> Position 0 0 --use 0 0 if startPos is not found (should never happen)
      Just val -> val);
    roomSize = getGridSize (tiles (grid nwRoom));
    combinedRoomTiles = combineTiles tilesExcistingRoom tilesNwRoom offset;
    resultTiles = combineTiles generatedLevelAllTiles combinedRoomTiles (positionToVector startPos);
    resultGeneratedLevel = GeneratedLevel (gl_name gLevel) (Grid resultTiles);
}-}

{-
addGridToLevel2::GeneratedLevel->Room->ObjectId->Vector2->GeneratedLevel
addGridToLevel2 gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId; --getTileData generatedLevelAllTiles startPos roomSize;
    -- tilesExcistingRoom = [[fst posTile | (x, posTile) <- zip[0..] row] | (y, row) <- zip[0..] tilePosExcistingRoom];
    tilesExcistingRoom = [[tile | (tile, pos) <- row] | (row) <- tilePosExcistingRoom];
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


addGridToLevelOLD::GeneratedLevel->Room->ObjectId->Vector2->GeneratedLevel
addGridToLevelOLD gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId; --getTileData generatedLevelAllTiles startPos roomSize;
    -- tilesExcistingRoom = [[fst posTile | (x, posTile) <- zip[0..] row] | (y, row) <- zip[0..] tilePosExcistingRoom];
    tilesExcistingRoom = [[tile | (tile, pos) <- row] | (row) <- tilePosExcistingRoom];
    tilesNwRoom = tiles (grid nwRoom);
    startPosMaybe = getOriginWorldPosition gLevel relativeToRoomId;
    startPos = (case startPosMaybe of
      Nothing  -> Position 0 0 --use 0 0 if startPos is not found (should never happen)
      Just val -> val);
    roomSize = getGridSize (tiles (grid nwRoom));
    combinedRoomTiles = combineTiles tilesExcistingRoom tilesNwRoom offset;
    resultTiles = combineTiles generatedLevelAllTiles combinedRoomTiles (positionToVector startPos);
    resultGeneratedLevel = GeneratedLevel (gl_name gLevel) (Grid resultTiles);
}

addGridToLevelOLD2::GeneratedLevel->Room->ObjectId->Vector2->GeneratedLevel
addGridToLevelOLD2 gLevel nwRoom relativeToRoomId offset = resultGeneratedLevel
  where{
    generatedLevelAllTiles = tiles (gl_grid gLevel);
    tilePosExcistingRoom = getTilesWithWorldPosition generatedLevelAllTiles relativeToRoomId; --getTileData generatedLevelAllTiles startPos roomSize;
    -- tilesExcistingRoom = [[fst posTile | (x, posTile) <- zip[0..] row] | (y, row) <- zip[0..] tilePosExcistingRoom];
    tilesExcistingRoom = [[tile | (tile, pos) <- row] | (row) <- tilePosExcistingRoom];
    tilesNwRoom = tiles (grid nwRoom);
    startPosMaybe = getOriginWorldPosition gLevel relativeToRoomId;
    startPos = (case startPosMaybe of
      Nothing  -> Position 0 0 --use 0 0 if startPos is not found (should never happen)
      Just val -> val);
    roomSize = getGridSize (tiles (grid nwRoom));
    combinedRoomTiles = combineTiles tilesExcistingRoom tilesNwRoom offset;
    finalStartPos = ( if(x startPos)>=0 then (x startPos) else (fst offset),
                      if(y startPos)>=0 then (y startPos) else (snd offset));
    resultTiles = combineTiles generatedLevelAllTiles combinedRoomTiles finalStartPos;
    resultGeneratedLevel = GeneratedLevel (gl_name gLevel) (Grid resultTiles);
}-}
