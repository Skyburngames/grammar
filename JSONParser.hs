-- Allow ourselves to write Text literals.
{-# LANGUAGE DeriveGeneric #-} -- must be set BEFORE 'module Main where'
{-# LANGUAGE OverloadedStrings #-} -- must be set BEFORE 'module Main where'

module JSONParser  
(
    generateJSON
) where 

-- ============================= IMPORTS =============================
import Grammar
import Control.Monad (when)
import Data.Aeson
import Data.Text(pack, unpack, replace)
import GHC.Generics


-- ============================= ToJSON =============================
instance ToJSON JSONParser.Game
instance ToJSON JSONParser.Level
instance ToJSON JSONParser.Room
instance ToJSON JSONParser.Grid
instance ToJSON JSONParser.Tile
instance ToJSON GridPositionJSON

instance ToJSON GameType
instance ToJSON TileType
instance ToJSON Entity
instance ToJSON EntityType
instance ToJSON ObjectId

-- ================================= JSON types ================================
data Game = Game {
    levels :: [JSONParser.Level],
    gameType :: GameType
} deriving (Show, Generic)


data Level = Level {
    name:: String,
    rooms:: [JSONParser.Room]
} deriving (Show, Generic)


data Room = Room {
    roomId:: ObjectId,
    grid:: JSONParser.Grid
} deriving (Show, Generic)


data Grid = Grid {
    tiles:: [JSONParser.Tile]
} deriving (Show, Generic)


data Tile = Tile {
    tileGridPosition:: GridPositionJSON,
    tileType:: TileType,
    entities:: [Entity]
} deriving (Show, Generic)


data GridPositionJSON = GridPositionJSON { -- only used in export to JSON
    x:: Int,
    y:: Int
} deriving (Show, Generic)


-- ================================ CONVERT =========================================

convertGame::Grammar.Game->JSONParser.Game
convertGame game = JSONParser.Game (map convertLevel (Grammar.levels game)) (Grammar.gameType game)

convertLevel::Grammar.Level->JSONParser.Level
convertLevel level = JSONParser.Level (Grammar.name level) (map convertRoom (Grammar.rooms level))

convertRoom::Grammar.Room->JSONParser.Room
convertRoom room = JSONParser.Room (Grammar.roomId room) (convertGrid (Grammar.grid room))

convertGrid::Grammar.Grid->JSONParser.Grid
convertGrid grid = JSONParser.Grid (convert2DTiles (Grammar.tiles grid))

convert2DTiles::[[Grammar.Tile]]->[JSONParser.Tile]
convert2DTiles [] = []
--convert2DTiles rows = [(convertTile tile x y) | (x,row) <- zip[0..] rows, (y,tile) <- zip[0..] row] 
convert2DTiles rows = [(convertTile tile x y) | (y,row) <- zip[0..] rows, (x,tile) <- zip[0..] row] 

convertTile::Grammar.Tile->Int->Int->JSONParser.Tile
convertTile tile posX posY = JSONParser.Tile (GridPositionJSON posX posY) (Grammar.tileType tile) (Grammar.entities tile)


-- ============================= Generate product.json =============================
generateJSON nwGame fileLocation = do
    -- contents <- readFile "file.txt"
    -- let newContents = show $ nwGame
    -- let newContents = (printGame nwGame)
    -- let parsedGame = show(encode(nwGame))
    let parsedGame = show(encode(convertGame nwGame))
    let unEscapedVersion = firstLast parsedGame
    let escapedVersion = replacedoubleslash unEscapedVersion

    when (Prelude.length escapedVersion > 0) $
        writeFile (fileLocation ++ "/product.json") escapedVersion
        

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = Prelude.tail (Prelude.init xs)


replacedoubleslash :: String -> String
replacedoubleslash = unpack . replace "\\" "" . pack


-- ============================= EXAMPLES =============================
-- data Person = Person { name :: Text, age :: Int } deriving (Show, Generic)
-- instance ToJSON Person
-- instance FromJSON Person