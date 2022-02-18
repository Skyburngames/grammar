{-
  This Module represents the Main, the generation is configured in here
-}

-- Use: 'cabal run' on this file location to run

module Main where

import Grammar
import JSONParser
import GridBuilders
import TileBuilders
import TileModifiers
import Conditions
import RoomConnectionGenerator
import GenerateLevel
import HelperCombinators

import RandomUtils
import System.Random


main :: IO ()
main = do
    -- ========================== GENERATE A PRODUCT ====================================---
    -- to make the process easier, set this up to export to the Asset folder of a game project
    generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- Use for generation on my (AW)PC
    -- generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Use for generation on my Laptop




-- ****************************** GENERATE DATASTRUCTURE ******************************
-- ---------------- the start stdGen ----------------
startGen = mkStdGen 8888 --create a starting stdGen (use a randomd nr here)


-- ============================================ START ROOMS ========================================
-- always add 1 startRoom

-- the room the player starts, set room id to 1
startPlayerRoom = runGridBuildersOnRoom startRoom startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (2,2) (2,2) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open))),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.6 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.35 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]
  where
    startRoom = createRoom 1 5 5 Open


-- ============================================ ROOMS ========================================
-- The rooms between the start and endRoom
randomRoom1 = createRandomRoomWithEnemy 2
randomRoom2 = createRandomRoom 3
randomRoom3 = createRandomRoomWithEnemy 4
randomRoom4 = createRandomRoom 5


-- ============================================ END ROOMS ========================================
-- These rooms are the finish for the player, always include 1 to the Level

-- endRoom: use simple (wrapper) combinators
endRoomSimpleCombinators = runGridBuildersOnRoom startRoom startGen [
    gb_makeAllTilesOpen,
    gb_createWall 1 1,
    gb_createWall 3 1,
    gb_createWall 2 3,
    gb_createWall 3 3,
    gb_addEntityToTile 3 2 finishEntity,
    gb_createBorderAroundRoom
  ]
  where
    roomId = 6
    startRoom = (createEmptyRoom roomId 5 5)
    finishEntity = createFinishEntity finishId
    finishId = 1

-- endRoom: use foundational combinators
endRoomFoundationalCombinators = runGridBuildersOnRoom endRoom startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open))),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]
  where
    endRoom = createRoom 6 5 5 Open


-- ================= Setup the connections =============================
-- connect the rooms
conA = connectRoomDown startPlayerRoom randomRoom1
conB = connectRoomLeft randomRoom1 randomRoom2
conC = connectRoomUp randomRoom2 randomRoom3
conD = connectRoomUp randomRoom3 randomRoom4
conE = connectRoomRight randomRoom4 endRoomSimpleCombinators



-- *************************************** GENERATE THE LEVEL ***************************************
-- create the RoomedLevel
roomedLevel = RoomedLevel "level1" [startPlayerRoom,randomRoom1, randomRoom2, randomRoom3, randomRoom4, endRoomSimpleCombinators] [conA, conB, conC, conD, conE]

-- ========== Create the level =======
levelWithDoors = openCriticalPathRoomedLevel roomedLevel
level1 = progressRoomedLevel levelWithDoors
game1 = Game [level1] TopDown



-- ============================== ROOM GENERATION FUNCTIONS ===========================================
-- ==================== Helper functions to quickly create a random room ==============================

createRandomRoom::Int->Room
createRandomRoom roomIdNr = runGridBuildersOnRoom (createRoom roomIdNr 5 5 Open) startGen [
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.2 Cumulative) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.2 Average) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.2 Average) (tb_editTile (setTileType Open)),
  gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Powerup)).(setTileType Open))),
  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]

-- A helper function to quickly create a random room with an Enemy
createRandomRoomWithEnemy::Int->Room
createRandomRoomWithEnemy roomIdNr = runGridBuildersOnRoom (createRoom roomIdNr 5 5 Open) startGen [
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Average) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Average) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Average) (tb_editTile (setTileType Open)),
  gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Enemy)).(setTileType Open))),

  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  -- gb_randomTile (3,3) (3,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open)))
  ]








-- ============================= EXAMPLE ROOMS are currently not used but show the possibilities =====================================
generatedRoom2 = runGridBuildersOnRoom _startRoom startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]
    where
      _startRoom = createRoom 2 5 5 Open

startRoom4 = createRoom 4 5 5 Open
generatedRoom4 = runGridBuildersOnRoom startRoom4 startGen [
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Solid)),

    gb_randomTile (2,2) (2,2) (tb_editTile ((addEntity (Entity (ObjectId 3) Enemy)).(setTileType Open))),
    gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open))),

    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
    -- gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 3) Enemy)).(setTileType Open)))
    --gb_randomTile (3,3) (2,2) (tb_editTile ((addEntity (Entity (ObjectId 4) Powerup)))),
    --gb_randomTile (2,2) (3,3) (tb_editTile ((addEntity (Entity (ObjectId 5) Powerup))))
    -- gb_allTiles (con_always True) (tb_editTile (setTileType Solid))
    -- gb_randomTile (1,1) (1,1) (tb_editTile (setTileType Open)),
    -- gb_randomTile (3,3) (1,1) (tb_editTile (setTileType Open)),
    -- gb_randomTile (1,1) (3,3) (tb_editTile (setTileType Open)),
    -- gb_randomTile (3,3) (3,3) (tb_editTile (setTileType Open)),
  ]
