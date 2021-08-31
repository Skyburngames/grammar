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
startGrid = Grid (generateTiles 5 5)
startGen = mkStdGen 902727
startOriginalData = (startGrid, startGen)

result1 = runMultipleGridBuilders startOriginalData [
  -- gb_randomTile (0,4) (0,4) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,9) (1,9)) (tb_editTile (setTileType Solid)),
  gb_randomTile (2,2) (2,2) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.6 Cumulative) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)),
  gb_randomTile (0,0) (0,0) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

result2 = runMultipleGridBuilders startOriginalData [
  gb_randomTile (1,4) (1,4) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,4) (1,4)) (tb_editTile (setTileType Solid)),
  --gb_allTiles (con_RandomPosition (1,4) (1,4)) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.5 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.4 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.3 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.2 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_neighbourTileCondition (con_isTileType Solid) 0.1 Cumulative) (tb_editTile (setTileType Solid)),
  gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  -- gb_allTiles (con_RandomPosition (2,3) (2,3)) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open)))
  ]

result3 = runMultipleGridBuilders startOriginalData [
    gb_allTiles (con_always True) (tb_editTile (setTileType Solid)),
    gb_randomTile (4,4) (2,2) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 1 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.5 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.4 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.3 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.2 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    gb_allTiles (con_neighbourTileCondition (con_isTileType Open) 0.1 Cumulative) (tb_editTile (setTileType Open)),
    gb_randomTile (4,4) (0,0) (tb_editTile ((addEntity (Entity (ObjectId 1) Player)).(setTileType Open))),
    gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid))
  ]

combinedGrids = combineGrids result1 result2 (15,12)
room1 = Room (ObjectId 2) combinedGrids

-- Create the level that is exported
level1 = Level "level1" [room1]
game1 = Game [level1] TopDown
