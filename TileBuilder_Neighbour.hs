module TileBuilder_Neighbour
(
    NeighbourData(..),
    --ChanceCalculation(..),
    createNeighbourData,
    editTileBasedOnNeighbour,
    chanceBasedOnConditionNeighbours,
) where

import Grammar
import RandomUtils
import System.Random
import TileBuilders
import TileModifiers
import GrammarV2

-- The main functions is called with a subfuction

-- ============================================== TileBuilder Function ========================================-
editTileBasedOnNeighbour::GridNeighbourBuilder->TileModifier->TileBuilder
editTileBasedOnNeighbour gridNeighbourBuilder tileFunc grid position tile gen =
    if (randomRoll (gridNeighbourBuilder grid nwNeighbourData gen2) gen)
        then tileFunc tile
        else tile
    where {
        nwNeighbourData = createNeighbourData position;
        gen2 = snd (split gen);
    }



-- ============================================== Config functions =========================================-
chanceBasedOnConditionNeighbours::TileCondition->Float->ChanceCalculation->GridNeighbourBuilder
chanceBasedOnConditionNeighbours condition chance chanceCalculation grid nbData gen =
    calculateChance [totalChanceLeft, totalChanceRight, totalChanceUp, totalChanceDown] chanceCalculation
    -- (totalChanceLeft + totalChanceRight + totalChanceUp + totalChanceDown)
    where {
        totalChanceLeft = getChancedBasedOnConditionMatch grid (left nbData) condition chance;
        totalChanceRight = getChancedBasedOnConditionMatch grid (right nbData) condition chance;
        totalChanceUp = getChancedBasedOnConditionMatch grid (up nbData) condition chance;
        totalChanceDown = getChancedBasedOnConditionMatch grid (down nbData) condition chance;
        -- currentTile = getTile grid (current nbData);

        getChancedBasedOnConditionMatch::Grid->Position->(Tile->Bool)->Float->Float;
        getChancedBasedOnConditionMatch grid2 pos con addChance =
            if(isPositionInGrid grid2 pos) then
                (if (con tile) then addChance
                else 0)
            else 0
            where {
                tile = getTile grid2 pos
            }
    }



-- ============================================== HELPER CLASSES ========================================-
type TileCondition = (Tile->Bool)
type GridNeighbourBuilder = (Grid->NeighbourData->StdGen->Float)
-- data ChanceCalculation = Highest| Cumulative | Average deriving (Show)

data NeighbourData = NeighbourData {
    current:: Position,
    left:: Position,
    right:: Position,
    up:: Position,
    down:: Position
} deriving (Show)

-- ============================================== HELPER FUNCTIONS ========================================-
createNeighbourData::Position->NeighbourData
createNeighbourData startPos = NeighbourData currentPos leftPos rightPos upPos downPos
    where {
        currentPos = startPos;
        posX = (x startPos);
        posY = (y startPos);
        leftPos = Position (posX-1) posY;
        rightPos = Position (posX+1) posY;
        upPos = Position posX (posY+1);
        downPos = Position posX (posY-1)
    }

calculateChance::[Float]->ChanceCalculation->Float
calculateChance [] _ = 0
calculateChance chanceList Highest = clamp01 (getMax chanceList)
calculateChance chanceList Cumulative = clamp01 (sum chanceList)
calculateChance chanceList Average = clamp01 (sum chanceList / toFloat (length chanceList))
