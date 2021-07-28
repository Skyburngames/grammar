module Main where

import Grammar
-- import GrammarToJSON
import JSONParser
import Generator

import RandomGenerator




main :: IO ()
-- main = print ("tile max width:" ++ newRand)
main = do 
    -- generateRandoms
    generateJSON game1 "C:/Users/kevin/PG_Game/Assets" -- AW-PW
    -- generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop
    {- print("test random: " ++ show(getRandomRange 1 10 1))
    print("test random: " ++ show(getRandomRange 1 10 2))
    print("test random: " ++ show(getRandomRange 3 22 3))
    print("test random: " ++ show(getRandomRange 1 10 4)) -}
    print("test")
-- main = generateJSON game1 "C:/Users/Kevin/Projecten/UnityProjecten/PG_Game/Assets" -- Laptop






-- ============================= Create the datastructure ===========================================

playerEntity = Entity (ObjectId 1) Player

originalTiles = generateTiles 8 8


generatedTiles = applyFuncsToGrid [
    --mapTileWithPosition (setTileTypeInPosition (Position 0 0) Solid),
    --mapTileWithPosition (setTileTypeInPosition (Position 1 1) Solid),
    --mapTileWithPosition (setTileTypeInPosition (Position 2 2) Solid)
    mapTileWithPosition (editTile (Position 0 0) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 1) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 2) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 3) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 4) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 5) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 6) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 0 7) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 7 0) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 7 1) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 7 2) (setTileType Solid)),
    mapTileWithPosition (editTile (Position 3 3) (addEntity (Entity (ObjectId 1) Player)))
    -- editTiles 2 3 (setTileType Solid),
    -- editTiles 2 2 (addEntity (Entity (ObjectId 1) Player))
    -- generateTiles (getRandomRange 8 14 0) (getRandomRange 8 14 1) [] --, 
    -- createSolidSquare (getRandomRange 1 3 2) (getRandomRange 1 3 3) (getRandomRange 2 5 4) (getRandomRange 2 5 5), 
    -- editTile (setTileType Solid) (GridPosition 7 7),
    -- editTile (setTileType Open) (GridPosition 4 4),
    -- editTile (addEntity playerEntity) (GridPosition 4 4),
    -- editTile (setTileType Open) (GridPosition 5 4)
    ] originalTiles

-- tiles2 = editTiles originalTiles 0 2 (setTileType Solid)
-- tiles3 = editTiles tiles2 2 2 (addEntity (Entity (ObjectId 1) Player))

--room1 = Room (ObjectId 2) generatedTiles

-- generatedTiles2 = mapTileWithPosition (setTileTypeInPosition (Position 4 4) Solid) generatedTiles

room1 = Room (ObjectId 2) (Grid generatedTiles)


-- Create the level that is exported
level1 = Level "level1" [room1]
game1 = Game [level1] TopDown 

