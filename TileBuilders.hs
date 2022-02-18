{-
  This module contains the TileBuilders
    - A TileBuilder is a function that generates a new Tile
-}

module TileBuilders
(
    -- tb_setTileType,
    tb_editTile
) where

import Grammar


-- ============================================ TILEBUILDERS =====================================================

tb_editTile::TileModifier->TileData->Tile
tb_editTile tileModifierFunc (_,_,t,_) = tileModifierFunc t




-- =========================== OLD ============================
{-
tb_setTileType::TileType->TileData->Tile
tb_setTileType nwTileType (originalData,p,t,gen) = Tile nwTileType (entities t)
-}
