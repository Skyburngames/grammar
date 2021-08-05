-- Use: 'cabal run' on this file location to run

module Main where

import Grammar
import JSONParser
import GridBuilders
import TileBuilders
import TileModifiers
import Conditions

import RandomUtils
import System.Random




main :: IO ()
main = do
    -- ========================== GENERATE A PRODUCT ====================================---
    -- generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- AW-PW
    generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop
    -- print(toFloat 20)
    --print(test (con_isTileType Solid) 0.25 Highest ((grid3, mkStdGen 54566), testPos1, (getTile grid3 testPos1), mkStdGen 123))


-- ====================================== GENERATE DATASTRUCTURE =================================================
startGrid = Grid (generateTiles 10 10)
startGen = mkStdGen 902727
startOriginalData = (startGrid, startGen)

result1 = runMultipleGridBuilders startOriginalData [
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 1 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_gridBorder) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (2,7) (2,7)) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

result2 = runMultipleGridBuilders startOriginalData [
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.5 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.4 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.3 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.2 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_gridBorder) (tb_editTile (setTileType Solid)),
  runGridBuilder (con_RandomPosition (2,7) (2,7)) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

result3 = runMultipleGridBuilders startOriginalData [
    runGridBuilder (con_always True) (tb_editTile (setTileType Solid)),
    runGridBuilder (con_RandomPosition (4,4) (6,6)) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 1 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.5 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.4 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.2 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    runGridBuilder (con_RandomPosition (4,4) (6,6)) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open))),
    runGridBuilder (con_gridBorder) (tb_editTile (setTileType Solid))
  ]


room1 = Room (ObjectId 2) result3

-- Create the level that is exported
level1 = Level "level1" [room1]
game1 = Game [level1] TopDown
