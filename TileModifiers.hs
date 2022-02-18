{-
    - Use these functions to Modify a excisting Tile
-}

module TileModifiers
(
    setTileType,
    addEntity,
) where

import Grammar


-- ============================================ TILEMODIFIERS =====================================================
setTileType::TileType->TileModifier
setTileType nwTileType = \r@(Tile {tileType = tp}) -> r {tileType=nwTileType}


addEntity::Entity->TileModifier
addEntity nwEntity = \r@(Tile {entities = ent}) -> r {entities=nwEntity:ent}
