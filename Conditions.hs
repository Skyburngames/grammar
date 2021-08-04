module Conditions
(
  con_position,
  con_RandomPosition,
  con_isWall,
  con_neighbourTileCondition,
) where

import Grammar
import System.Random
import RandomUtils


-- ============================================ CONDITIONS =====================================================
con_position::Position->InputData->Bool
con_position requiredPosition (originalData,p,t,gen) = (isPosition p requiredPosition)

con_isWall::InputData->Bool
con_isWall (_,_,t,_) = isWall t


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
}

-- ============================================= PRIVATE =============================================================
calculateTotalChance::[Float]->ChanceCalculation->Float
calculateTotalChance [] _ = 0
calculateTotalChance chanceList Highest = clamp01 (getMax chanceList)
calculateTotalChance chanceList Cumulative = clamp01 (sum chanceList)
calculateTotalChance chanceList Average = clamp01 (sum chanceList / toFloat (length chanceList))
