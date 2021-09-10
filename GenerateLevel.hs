{-
  This Module represents the Gridlayer. Use this module to combine Level->Room->(RoomConnector | Tile) to GeneratedLevel->Tile
-}

module GenerateLevel
(
  progressLevel
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
  if(doesRoomExcist gLevel roomTodo) then generateLevel gLevel level nwRoomsTodoRemaining
    else generateLevel nwGeneratedLevel level nwRoomsTodoRemaining
  where {
    nwGeneratedLevel = generateRoomInGeneratedLevel gLevel roomTodo (roomConnections level); -- generate the room
    -- nwGeneratedLevel = generateAllConnectedRooms gLevel roomTodo (roomConnections level);
    -- nwRoomsTodoRemaining = connectedRoomsNotYetMade ++ otherRoomsTodo; --
    nwRoomsTodoRemaining = [ r|r<-allTodoRooms, not (doesRoomExcist nwGeneratedLevel r)]; -- filter on rooms that arent made yet
    allTodoRooms = connectedRooms++otherRoomsTodo; --combine the connected rooms with the rooms that where already todo
    connectedRooms = (getConnectedRooms level roomTodo); -- all rooms connected to roomTodo
}


-- addToFrontIfNotAlreadyInList::[Room]->[Room]->[Room]
-- addIfNotAlreadyInList roomsA roomsB = roomsA ++ roomsB --TODO


getConnectedRooms::Level->Room->[Room]
getConnectedRooms level room = [] --TODO



generateRoomInGeneratedLevel::GeneratedLevel->Room->[RoomConnector]->GeneratedLevel --make this room but make sure it is located on the valid position based on roomConnectors and the current generatedLevel
generateRoomInGeneratedLevel gLevel room roomConnectors = gLevel --TODO

--generateRoomAndAllConnectedRooms::GeneratedLevel->Room->[RoomConnector]->GeneratedLevel -- check if connected room already excist
--generateRoomAndAllConnectedRooms gLevel room roomConnectors = gLevel --TODO




doesRoomExcist::GeneratedLevel->Room->Bool
doesRoomExcist gLevel room = True --TODO



addRoomToGeneratedLevel::GeneratedLevel->[RoomConnector]->Room->GeneratedLevel
addRoomToGeneratedLevel startGLevel roomConnectors room = startGLevel --TODO
