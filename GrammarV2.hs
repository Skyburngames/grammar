module GrammarV2
(
  runGridBuilder2,
  con_position,
  tb_setTileType,
  con_RandomPosition
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



-- Loop over all tiles a call the TileCondition function to check if the action should be performed on this tile,then call the tileBuilder function to alter the tile
runGridBuilder2::Bool->(TileCondition)->(TileBuilder)->Grid->StdGen->Grid
runGridBuilder2 uniqueGenCondition tileConFunc tileBuilderFunc startGrid startGen = Grid nwTiles
  where{
    nwTiles = [[progressTile tile (Position x y) | (x, tile) <- zip[0..] row] | (y, row) <- zip[0..] (tiles startGrid)];

    -- Progress a tile based on it position
    -- Based on uniqueGenCondition: either use the same StdGen in EVERY tileCondition function or generate a new StdGen for each TileCondition
    progressTile::Tile->Position->Tile;
    progressTile tile pos = if(uniqueGenCondition)
      then progressTileBasedOnCondition (getGeneratorCon pos) (getGeneratorTB pos) tile pos
      else progressTileBasedOnCondition startGenCon startGenTB tile pos;

    -- if the condition function is true then do the tileBuilder function or else use the default tile without altering it.
    -- 2 StdGens are required, these are used as the StdGen for the tileCondition-function and the tileBuilder-function.
    progressTileBasedOnCondition::StdGen->StdGen->Tile->Position->Tile;
    progressTileBasedOnCondition genCond genTB progressedTile pos = if(tileConFunc (startGrid, pos, progressedTile, genCond))
      then tileBuilderFunc (startGrid, pos, progressedTile, genTB) else progressedTile;

    -- generate 2 new StdGen that are used as the starting points for the tileCondition-functions and the tilebuilder-functions
    startGenCon = fst (split startGen);
    startGenTB = snd (split startGen);

    -- required basic info
    gridWidth = getGridWidth startGrid;
    totalTiles = gridWidth * (getGridHeight startGrid);

    -- generate totalTiles* StdGen, twice, once for the TileCondition-functions and once for the TileBuilder-functions
    generatorsCon = createGenerators totalTiles startGenCon;
    generatorsTileBuilders = createGenerators totalTiles startGenTB;

    -- functions to retrieve a StdGen based on the position
    getGeneratorCon::Position->StdGen;
    --getGenerator1 pos1 = generators!!((yPos * gridWidth) + xPos);
    getGeneratorCon pos1 = generatorsCon!!(((y pos1) * gridWidth) + (x pos1));
    getGeneratorTB::Position->StdGen;
    --getGenerator2 pos2 = generators!!(((yPos * gridWidth) + xPos) + totalTiles)
    getGeneratorTB pos2 = generatorsTileBuilders!!(((y pos2) * gridWidth) + (x pos2))
  }


con_position::Position->TilePosData->Bool
con_position requiredPosition (g,p,t,gen) = if (isPosition p requiredPosition) then True else False

con_RandomPosition::Vector2->Vector2->TilePosData->Bool
con_RandomPosition rangeX rangeY (g,p,t,gen) = con_position randomPos (g,p,t,gen)
  where {
    randomPos = Position randomX randomY;
    gen2 = snd (splitStdGen gen);
    randomX = fst (getRandomValue (fst rangeX) (snd rangeX) gen);
    randomY = fst (getRandomValue (fst rangeY) (snd rangeY) gen2);
}

tb_setTileType::TileType->TilePosData->Tile
tb_setTileType nwTileType (g,p,t,gen) = Tile nwTileType (entities t)
