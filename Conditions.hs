module Conditions
(
  con_position,
  -- con_RandomPosition,
  con_isTileType,
  con_gridBorder,
  con_always,
  con_neighbourTileCondition,
) where

import Grammar
import System.Random
import RandomUtils


-- ============================================ CONDITIONS =====================================================
con_position::Position->InputData->Bool
con_position requiredPosition (_,p,t,gen) = (isPosition p requiredPosition)

con_always::Bool->InputData->Bool
con_always alwaysBool (_,_,_,_) = alwaysBool

con_isTileType::TileType->InputData->Bool
con_isTileType tt (_,_,t,_) = compareTileTypes tt (tileType t)

con_gridBorder::InputData->Bool
con_gridBorder ((originalGrid),position,_,_) = isLeftBorder || isRightBorder || isTopBorder || isBottomBorder
  where{
    isLeftBorder = (x position) == 0;
    isRightBorder = (x position) == ((getGridWidth (tiles originalGrid))-1);
    isTopBorder = (y position) == ((getGridHeight (tiles originalGrid))-1);
    isBottomBorder = (y position) == 0;
}


-- foreach neighbour: if the tileCondition the chance is used, the final calculation is based on the chanceCalc-setting
con_neighbourTileCondition::TileCondition->Float->ChanceCalculation->InputData->Bool
con_neighbourTileCondition tileConFunc chance chanceCalc (grid,pos,tile,gen) = randomRoll totalChance gen
  where {
    totalChance = calculateTotalChance [chanceLeft, chanceRight, chanceUp, chanceDown] chanceCalc;
    generators = createGenerators 4 gen;
    chanceLeft = getChanceBasedOnTileCondition (-1,0) (generators!!0);
    chanceRight = getChanceBasedOnTileCondition (1,0) (generators!!1);
    chanceUp = getChanceBasedOnTileCondition (0,1) (generators!!2);
    chanceDown = getChanceBasedOnTileCondition (0,-1) (generators!!3);
    getChanceBasedOnTileCondition::Vector2->StdGen->Float;
    getChanceBasedOnTileCondition offset currentGen = if(tileConFunc (grid, offsetPos, offsetTile, currentGen)) then chance else 0
      where {
        offsetPos = getPositionRelative pos offset;
        offsetTile = getTileRelative grid pos offset;
    }
}

{-
con_RandomPosition::Vector2->Vector2->InputData->Bool
con_RandomPosition rangeX rangeY ((originalGrid, originalGen),p,t,gen) = con_position randomPos ((originalGrid, originalGen),p,t,gen)
  where{
    randomPos = (randomPosition rangeX rangeY originalGen)
  }



-- foreach neighbour: if the tileCondition the chance is used, the final calculation is based on the chanceCalc-setting
con_neighbourTileCondition::TileCondition->Float->ChanceCalculation->InputData->Bool
con_neighbourTileCondition tileConFunc chance chanceCalc ((originalGrid, originalGen),pos,tile,gen) = randomRoll totalChance gen
  where {
    totalChance = calculateTotalChance [chanceLeft, chanceRight, chanceUp, chanceDown] chanceCalc;
    generators = createGenerators 4 gen;
    chanceLeft = getChanceBasedOnTileCondition (-1,0) (generators!!0);
    chanceRight = getChanceBasedOnTileCondition (1,0) (generators!!1);
    chanceUp = getChanceBasedOnTileCondition (0,1) (generators!!2);
    chanceDown = getChanceBasedOnTileCondition (0,-1) (generators!!3);
    getChanceBasedOnTileCondition::Vector2->StdGen->Float;
    getChanceBasedOnTileCondition offset currentGen = if(tileConFunc ((originalGrid, originalGen), offsetPos, offsetTile, currentGen)) then chance else 0
      where {
        offsetPos = getPositionRelative pos offset;
        offsetTile = getTileRelative originalGrid pos offset;
    }
}-}

-- ============================================= PRIVATE =============================================================
calculateTotalChance::[Float]->ChanceCalculation->Float
calculateTotalChance [] _ = 0
calculateTotalChance chanceList Highest = clamp01 (getMax chanceList)
calculateTotalChance chanceList Cumulative = clamp01 (sum chanceList)
calculateTotalChance chanceList Average = clamp01 (sum chanceList / toFloat (length chanceList))
