module GrammarV2
(
  runGridBuilder2,
  con_position,
  tb_setTileType
) where

import Grammar
import System.Random
import RandomUtils
import GridBuilders


-- ======================================== HELPER CLASSES ==========================================
type TilePosData = (Grid,Position,Tile,StdGen)
type TileCondition = TilePosData->Bool
type TileBuilder = TilePosData->Tile


runGridBuilderNOTGOOD::(TileCondition)->(TileBuilder)->Grid->StdGen->Grid
runGridBuilderNOTGOOD tileCon tileBuilder startGrid startGen = Grid []
  where {
  tilePosDataList::[[TilePosData]];
  tilePosDataList = [[(startGrid, (Position x y), tile, (getGenerator generators x y)) --[[tileBuilder grid (Position x y) tile (getGenerator generators x y)--(mkStdGen ((x * 2) * (y * 9)))
      | (x, tile) <- zip[0..] row]
      | (y, row) <- zip[0..] (tiles startGrid)];
  gridWidth = getGridWidth startGrid;
  totalTiles = gridWidth * (getGridHeight startGrid);
  generators = createGenerators totalTiles startGen;
  getGenerator::[StdGen]->Int->Int->StdGen;
  getGenerator generators xPos yPos = generators!!((yPos * gridWidth) + xPos)
    where {
      gridWidth = getGridWidth startGrid;
      totalTiles = gridWidth * (getGridHeight startGrid)
    }
  }


-- IDEA: Loop over all tiles a call the TileCondition function to check if the action should be performed on this tile,then call the tileBuilder function to alter the tile
runGridBuilder2::(TileCondition)->(TileBuilder)->Grid->StdGen->Grid
runGridBuilder2 tileConFunc tileBuilderFunc startGrid startGen = Grid []


con_position::Position->TilePosData->Bool
con_position requiredPosition (g,p,t,gen) = if (isPosition p requiredPosition) then True else False

tb_setTileType::TileType->TilePosData->Tile
tb_setTileType nwTileType (g,p,t,gen) = Tile nwTileType (entities t)
