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
    --print(test con_isWall 0.25 Highest ((grid3, mkStdGen 54566), testPos1, (getTile grid3 testPos1), mkStdGen 123))


-- ====================================== GENERATE DATASTRUCTURE =================================================
startGrid = Grid (generateTiles 5 5)
startGen = mkStdGen 68302
startOriginalData = (startGrid, startGen)

--finalGrid2 = runGridBuilder2 (con_position (Position 2 4)) (tb_setTileType Solid) grid1 (mkStdGen 23983)
-- grid2 = runGridBuilder2 (con_RandomPosition (1,1) (1,1)) (tb_setTileType Solid) (startGrid, (mkStdGen 54566))
-- grid3 = runGridBuilder2 (con_RandomPosition (2,2) (2,2)) (tb_setTileType Solid) (grid2, (mkStdGen 8121))
-- grid4 = runGridBuilder2 (con_neighbourTileCondition con_isWall 0.25 Cumulative) (tb_setTileType Solid) (grid3, (mkStdGen 764))

finalGrid = runMultipleGridBuilders2 startOriginalData [
  runGridBuilder2 (con_RandomPosition (1,1) (1,1)) (tb_setTileType Solid),
  runGridBuilder2 (con_RandomPosition (2,2) (2,2)) (tb_setTileType Solid),
  runGridBuilder2 (con_RandomPosition (0,0) (4,4)) (tb_editTile (addEntity (Entity (ObjectId 1) Player))),
  runGridBuilder2 (con_neighbourTileCondition con_isWall 1 Cumulative) (tb_setTileType Solid)]

room1 = Room (ObjectId 2) finalGrid

-- Create the level that is exported
level1 = Level "level1" [room1]
game1 = Game [level1] TopDown
