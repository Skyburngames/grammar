{-
    - Use these functions to Generate a TileBuilder
    - A TileBuilder is a function that generates a new Tile
-}

module TileBuilders
(
    -- tb_setTileType,
    tb_editTile
) where

import Grammar




-- ============================================ TILEBUILDERS =====================================================
{-
tb_setTileType::TileType->InputData->Tile
tb_setTileType nwTileType (originalData,p,t,gen) = Tile nwTileType (entities t)
-}

tb_editTile::TileModifier->InputData->Tile
tb_editTile tileModifierFunc (_,_,t,_) = tileModifierFunc t
