{-
  This Module represents the Gridlayer. Use this module to combine Level->Room->(RoomConnector | Tile) to GeneratedLevel->Tile
-}

module GenerateLevel
(
  generateLevel
) where

import Grammar


generateLevel::Level->GeneratedLevel
generateLevel level = GeneratedLevel (name level) (grid ((rooms level)!!0))
