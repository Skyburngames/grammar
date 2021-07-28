{-
    - Use these functions to Generate a GridBuilder
    - A GridBuilder is a function that generates a new Grid
-}

module GridBuilders
(
    runMultipleGridBuilders,
    loopOverTiles,
    --generateRandomPosition,

) where

import Grammar
import System.Random
import TileBuilders
import RandomUtils


type GridBuilder = StdGen->Grid->Grid
-- type Condition = Grid->StdGen->[Position]



runMultipleGridBuilders::[(GridBuilder)]->GridBuilder
runMultipleGridBuilders [] _ startGrid = startGrid
runMultipleGridBuilders (currentFunction: otherFunctions) startGen startGrid = runMultipleGridBuilders otherFunctions gen2 (currentFunction startGen startGrid)
    where {
        gen2 = snd (splitStdGen startGen);
    }


loopOverTiles::TileBuilder->GridBuilder
loopOverTiles tileBuilder gen grid@(Grid {}) =
    grid {
        tiles = [[tileBuilder grid (Position x y) tile (getGenerator generators x y)--(mkStdGen ((x * 2) * (y * 9)))
            | (x, tile) <- zip[0..] row]
            | (y, row) <- zip[0..] (tiles grid)]
    } where {
        -- NOTE: create [stdGen] where each entry is a unique stdGen, then for each tile use one of these stdGen
        gridWidth = getGridWidth grid;
        totalTiles = gridWidth * (getGridHeight grid);
        generators = createGenerators totalTiles gen;
        getGenerator::[StdGen]->Int->Int->StdGen;
        getGenerator generators xPos yPos = generators!!((yPos * gridWidth) + xPos)
            where {
            gridWidth = getGridWidth grid;
            totalTiles = gridWidth * (getGridHeight grid)
        }
    }

--generateRandomPosition::StdGen->Position
--generateRandomPosition gen = Position 2 2

{-
createGenerators::Grid->Int->StdGen->[StdGen]
createGenerators _ 0 _ = []
createGenerators grid rem g = g:(createGenerators grid (rem-1) (snd (split g)))
-}

{-
loopOverTilesWithCondition::Condition->TileBuilder->GridBuilder
loopOverTilesWithCondition condition tileBuilder gen grid@(Grid {}) =
    grid {
        tiles = [[tileBuilder grid (Position x y) tile (getGenerator generators x y)--(mkStdGen ((x * 2) * (y * 9)))
            | (x, tile) <- zip[0..] row]
            | (y, row) <- zip[0..] (tiles grid)]
}

condition_allTiles::Condition
condition_allTiles grid gen = []
-}

 -- ================================================== PRIVATE ====================================================== --
