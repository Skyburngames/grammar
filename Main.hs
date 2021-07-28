-- Use: 'cabal run' on this file location to run

{-
Global Working:
    - run applyFuncsToGrid4
-}


module Main where

import Grammar
-- import GrammarToJSON
import JSONParser
-- import Generator
import GridBuilders
import TileBuilders
import TileModifiers

import RandomUtils
import TileBuilder_Neighbour
import System.Random




main :: IO ()
-- main = print ("tile max width:" ++ newRand)
main = do
    -- ========================== GENERATE A PRODUCT ====================================---
    generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- AW-PW
    -- generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop
    print(toFloat 20)
-- ============================= Create the datastructure ===========================================
grid1 = Grid (generateTiles 5 5)
startGen = mkStdGen 68302

finalGrid = runMultipleGridBuilders [
    -- loopOverTiles (editRandomTile (1,4) (2,2) (setTileType Solid)),
    --loopOverTiles (editRandomTile (3,3) (2,2) (setTileType Solid)),
    -- loopOverTiles (editRandomTile (1,4) (3,3) (setTileType Solid)),
    -- loopOverTiles (editRandomTile (1,4) (4,4) (setTileType Solid)),
    loopOverTiles (editTile (setTileType Solid) (Position 1 1)),
    loopOverTiles (editTile (setTileType Solid) (Position 0 4)),
    loopOverTiles (editTile (setTileType Solid) (Position 3 4)),
    -- loopOverTilesRandom (loopOverTiles (editTile (setTileType Solid))),

    -- loopOverTiles (editTileBasedOnNeighbour (chanceBasedOnConditionNeighbours (isWall) 0.5 Highest ) (setTileType Solid)),
    --loopOverTiles (editTileBasedOnNeighbour (chanceBasedOnConditionNeighbours (isWall) 0.3 Average) (setTileType Solid)),
    --loopOverTiles (editTileBasedOnNeighbour (chanceBasedOnConditionNeighbours (isWall) 0.3 Average) (setTileType Solid)),
    --loopOverTiles (editTileBasedOnNeighbour (chanceBasedOnConditionNeighbours (isWall) 0.2) (setTileType Solid)),

    loopOverTiles (editTile (addEntity (Entity (ObjectId 1) Player)) (Position 0 0))
    ] startGen grid1

room1 = Room (ObjectId 2) finalGrid

-- Create the level that is exported
level1 = Level "level1" [room1]
game1 = Game [level1] TopDown
