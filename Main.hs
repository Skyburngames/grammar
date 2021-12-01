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

import RandomUtils
import System.Random




main :: IO ()
main = do
    -- ========================== GENERATE A PRODUCT ====================================---
    -- generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- AW-PW
    -- print (getDoorPositionsInRoom (roomConnections roomedlevel3) generatedRoom1)
    generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop
    -- print(toFloat 20)
    --print(test (con_isTileType Solid) 0.25 Highest ((grid3, mkStdGen 54566), testPos1, (getTile grid3 testPos1), mkStdGen 123))
    -- print afterAddingRoom2TEST
    -- print tilesR2
    -- print testPrint1
-- ====================================== GENERATE DATASTRUCTURE =================================================
------------------------------------------ TEST SUITE ------------------------------------------

------------------------------------------------------------------------------------------


{-
startGrid = Grid (generateTiles 5 5)
startGen = mkStdGen 902727
startOriginalData = (startGrid, startGen)

grid1 = runMultipleGridBuilders startOriginalData [
  -- gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  -- gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),

  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),
  gb_randomTile (1,1) (1,1) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

grid2 = runMultipleGridBuilders startOriginalData [
  gb_randomTile (2,2) (4,4) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,4) (1,4)) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,4) (1,4)) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.5 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.4 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  --gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.3 Cumulative) (tb_editTile (setTileType Solid)),
  -- gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.2 Cumulative) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  -- gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  -- gb_allTiles (con_RandomPosition (2,3) (2,3)) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

grid3 = runMultipleGridBuilders startOriginalData [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 1 Cumulative) (tb_editTile (setTileType Open)),
    -- gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 1 Cumulative) (tb_editTile (setTileType Open)),
    -- gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.4 Cumulative) (tb_editTile (setTileType Open)),
    --gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    --gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.2 Cumulative) (tb_editTile (setTileType Open)),
    --gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    -- gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    -- gb_randomTile (4,4) (0,0) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open))),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]

grid4 = runMultipleGridBuilders startOriginalData [
    gb_randomTile (1,1) (1,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (3,3) (1,4) (tb_editTile (setTileType Solid)),
    gb_randomTile (1,4) (1,1) (tb_editTile (setTileType Solid)),
    --gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.4 Cumulative) (tb_editTile (setTileType Solid)),
    --gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.4 Cumulative) (tb_editTile (setTileType Solid))
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
    ]
-}
-- combinedGrids = combineGrids result1 result2 (15,12)

--generatedRoom1 = Room (ObjectId 1) r1
--generatedRoom2 = Room (ObjectId 2) r2
--generatedRoom3 = Room (ObjectId 3) r3
--generatedRoom4 = Room (ObjectId 4) r4

startGen = mkStdGen 323

startRoom1 = createRoom 1 5 5 Open
startRoom2 = createRoom 2 5 5 Open
startRoom3 = createRoom 3 5 5 Open
startRoom4 = createRoom 4 5 5 Open


startRoom = runGridBuildersOnRoom startRoom1 startGen [
  --gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Solid)),
  -- gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
  gb_randomTile (2,2) (2,2) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open))),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.6 Cumulative) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.35 Cumulative) (tb_editTile (setTileType Open)),
  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  -- gb_randomTile (3,3) (3,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open)))
  ]

generatedRoom2 = runGridBuildersOnRoom startRoom2 startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]
endRoom = runGridBuildersOnRoom startRoom3 startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open))),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]
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



createRandomRoom::Int->Room
createRandomRoom roomIdNr = runGridBuildersOnRoom (createRoom roomIdNr 5 5 Solid) startGen [
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.8 Average) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.8 Average) (tb_editTile (setTileType Open)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.8 Average) (tb_editTile (setTileType Open)),
  gb_randomTile (1,3) (1,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Powerup)).(setTileType Open))),

  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  -- gb_randomTile (3,3) (3,3) (tb_editTile ((addEntity (Entity (ObjectId 1) Finish)).(setTileType Open)))
  ]

randomRoom1 = createRandomRoom 5
randomRoom2 = createRandomRoom 6
randomRoom3 = createRandomRoom 7
randomRoom4 = createRandomRoom 8
randomRoom5 = createRandomRoom 9
randomRoom6 = createRandomRoom 10
randomRoom7 = createRandomRoom 11
randomRoom8 = createRandomRoom 12




{-
conA = connectRoomHorizontal startRoom randomRoom1
conB = connectRoomVertical randomRoom1 randomRoom4
conC = connectRoomHorizontal randomRoom4 endRoom
conD = connectRoomVertical startRoom randomRoom3
conE = connectRoomVertical endRoom randomRoom2
-}

--6pack
{-
conA = connectRoomHorizontal startRoom randomRoom1
conB = connectRoomVertical randomRoom1 randomRoom4
conC = connectRoomHorizontal randomRoom4 endRoom
conD = connectRoomVertical startRoom randomRoom3
conE = connectRoomHorizontal randomRoom1 randomRoom2
-}

--rocket
conA = connectRoomVertical startRoom randomRoom1
conB = connectRoomHorizontal randomRoom1 randomRoom2
conC = connectRoomHorizontal randomRoom2 randomRoom3
conD = connectRoomVertical randomRoom3 randomRoom4
conE = connectRoomHorizontal randomRoom4 randomRoom5
conF = connectRoomHorizontal randomRoom3 randomRoom6
conG = connectRoomVertical randomRoom5 endRoom

conH = connectRoomHorizontal randomRoom7 endRoom
conI = connectRoomHorizontal randomRoom8 randomRoom7




-- TEST 4 ROOMS
-- Create the level that is exported
--roomedLevel1 = RoomedLevel "level1" [startRoom, generatedRoom2, endRoom, generatedRoom4] [conA, conB, conC]
--roomedLevel2 = RoomedLevel "level2" [startRoom, generatedRoom2, generatedRoom4] [conA, conC]
--roomedLevel3 = RoomedLevel "level3" [startRoom] [conA]
roomedLevel4 = RoomedLevel "level4" [startRoom, randomRoom1, randomRoom2, randomRoom3, randomRoom4, randomRoom5, randomRoom6, endRoom] [conA, conB, conC, conD, conE, conF,conG]




levelWithDoors = openCriticalPathRoomedLevel roomedLevel4
level1 = progressRoomedLevel levelWithDoors

-- ------- TEST -----------
-- grid1 = tiles (gl_grid level1)
-- grid2 = combineTiles grid1 (tiles (grid generatedRoom3)) (-5,0)
-- resultGL = addGridToGeneratedLevel level1 generatedRoom3 (roomId generatedRoom2) ((-5),0)
-- testPrint1 = addGridToGeneratedLevel level1 generatedRoom3 (roomId generatedRoom2) ((-5),0)
-- resultGL = GeneratedLevel "level3" (Grid grid2)
-- ------- ------ -----------

game1 = Game [level1] TopDown
