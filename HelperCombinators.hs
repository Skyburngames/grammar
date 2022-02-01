module HelperCombinators
(
  gb_editAllTiles,
  gb_editTile,
  gb_editTileType,
  gb_editAllTileTypes,
  gb_addEntityToTile,
  createFinishEntity,
  createEmptyRoom,
  gb_createWall,
  gb_makeAllTilesOpen,
  gb_makeAllTilesSolid,
  gb_createBorderAroundRoom
) where

import Grammar
import System.Random
import RandomUtils
import Conditions
import TileBuilders
import GridBuilders
import TileModifiers

-- ====================================== CONVIENIENT functions ===================================

--edit a specific tile
gb_editTile::Int->Int->(TileBuilder)->GridData->Grid
gb_editTile posX posY tileBuilderFunc (startGrid, originalGen) = gb_randomTile (posX, posX) (posY, posY) tileBuilderFunc (startGrid, originalGen)

gb_editTileType::Int->Int->TileType->GridData->Grid
gb_editTileType posX posY tileType (startGrid, originalGen) = gb_editTile posX posY (tb_editTile (setTileType tileType)) (startGrid, originalGen)

gb_createWall::Int->Int->GridData->Grid
gb_createWall posX posY gridData = gb_editTileType posX posY Solid gridData

gb_addEntityToTile::Int->Int->Entity->GridData->Grid
gb_addEntityToTile posX posY entity gridData = gb_editTile posX posY (tb_editTile (addEntity entity)) gridData

-- edit all tiles
gb_editAllTiles::(TileBuilder)->GridData->Grid
gb_editAllTiles tileBuilderFunc (startGrid,startGen) = gb_allTiles (con_always True) tileBuilderFunc (startGrid,startGen)

gb_editAllTileTypes::TileType->GridData->Grid
gb_editAllTileTypes tileType (startGrid,startGen) = gb_editAllTiles (tb_editTile (setTileType tileType)) (startGrid,startGen)

gb_makeAllTilesOpen::GridData->Grid
gb_makeAllTilesOpen gridData = gb_editAllTileTypes Open gridData

gb_makeAllTilesSolid::GridData->Grid
gb_makeAllTilesSolid gridData = gb_editAllTileTypes Solid gridData

gb_createBorderAroundRoom::GridData->Grid
gb_createBorderAroundRoom gridData = gb_allTiles (con_gridBorder) (tb_editTile (setTileType Solid)) gridData

-- entities
createFinishEntity::Int->Entity
createFinishEntity objectId = Entity (ObjectId objectId) Finish

-- misc
createEmptyRoom::Int->Int->Int->Room
createEmptyRoom roomId width height = createRoom roomId width height Open
