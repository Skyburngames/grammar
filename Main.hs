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
    generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- AW-PW
    --generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop
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

startGen = mkStdGen 902723

startRoom1 = createRoom 1 5 5
startRoom2 = createRoom 2 5 5
startRoom3 = createRoom 3 5 5
startRoom4 = createRoom 4 5 5

generatedRoom1 = runGridBuildersOnRoom startRoom1 startGen [
  gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),
  gb_randomTile (1,1) (1,1) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

generatedRoom2 = runGridBuildersOnRoom startRoom2 startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (1,1) (2,2) (tb_editTile (setTileType Open)),
    gb_randomTile (3,3) (2,2) (tb_editTile (setTileType Open))
  ]
generatedRoom3 = runGridBuildersOnRoom startRoom3 startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (1,1) (1,1) (tb_editTile (setTileType Open)),
    gb_randomTile (3,3) (1,1) (tb_editTile (setTileType Open)),
    gb_randomTile (2,2) (3,3) (tb_editTile (setTileType Open))
  ]
generatedRoom4 = runGridBuildersOnRoom startRoom4 startGen [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (1,1) (1,1) (tb_editTile (setTileType Open)),
    gb_randomTile (3,3) (1,1) (tb_editTile (setTileType Open)),
    gb_randomTile (1,1) (3,3) (tb_editTile (setTileType Open)),
    gb_randomTile (3,3) (3,3) (tb_editTile (setTileType Open))
  ]




conA = connectRoomHorizontal generatedRoom1 generatedRoom2
conB = connectRoomHorizontal generatedRoom2 generatedRoom3
conC = connectRoomVertical generatedRoom2 generatedRoom4
-- conD = connectRoomVertical generatedRoom3 generatedRoom4

-- Create the level that is exported
fullyConnectedLevel = Level "level1" [generatedRoom1, generatedRoom2, generatedRoom3, generatedRoom4] [conA, conB, conC]
testConnectedLevel = Level "level2" [generatedRoom1, generatedRoom2] [conA]




generatedLevel1 = progressLevel fullyConnectedLevel

-- ------- TEST -----------
-- grid1 = tiles (gl_grid generatedLevel1)
-- grid2 = combineTiles grid1 (tiles (grid generatedRoom3)) (-5,0)
-- resultGL = addGridToGeneratedLevel generatedLevel1 generatedRoom3 (roomId generatedRoom2) ((-5),0)
-- testPrint1 = addGridToGeneratedLevel generatedLevel1 generatedRoom3 (roomId generatedRoom2) ((-5),0)
-- resultGL = GeneratedLevel "level3" (Grid grid2)
-- ------- ------ -----------

game1 = Game [generatedLevel1] TopDown
