module GridBuilders
(
  runMultipleGridBuilders,
  runGridBuilder,
) where

import Grammar
import System.Random
import RandomUtils



-- ======================================== GRIDBUILDERS ==========================================
runMultipleGridBuilders::OriginalData->[(OriginalData->Grid)]->Grid
runMultipleGridBuilders (startGrid,_) [] = startGrid
runMultipleGridBuilders (startGrid,startGen) (f:fs) = runMultipleGridBuilders (nwGrid, nextGen) fs
  where{
    nwGrid = f (startGrid, startGen);
    nextGen = snd (split startGen)
  }


-- Loop over all tiles a call the TileCondition function to check if the action should be performed on this tile,then call the tileBuilder function to alter the tile
runGridBuilder::(TileCondition)->(TileBuilder)->OriginalData->Grid
runGridBuilder tileConFunc tileBuilderFunc (startGrid,startGen) = Grid nwTiles
  where{
    nwTiles = [[progressTile tile (Position x y) | (x, tile) <- zip[0..] row] | (y, row) <- zip[0..] (tiles startGrid)];

    -- Progress a tile based on it position
    -- each call to tileConFunc is provided with a InputData in it there is a StdGen that is always the same for ALL calls to tileConFunc and there is a unique StdGen
    --  vica-versa for TileBuilder
    progressTile::Tile->Position->Tile;
    progressTile tile pos = if(tileConFunc ((startGrid, startGenCon), pos, tile, getGeneratorCon pos))
      then tileBuilderFunc ((startGrid, startGenTB), pos, tile, getGeneratorTB pos)
      else tile;

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
