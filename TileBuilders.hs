{-
    - Use these functions to Generate a TileBuilder
    - A TileBuilder is a function that generates a new Tile
-}

module TileBuilders
(
    TileBuilder(..),
    editRandomTile,
    editTile
) where

import Grammar
import System.Random
import RandomUtils
import TileModifiers



type TileBuilder = Grid->Position->Tile->StdGen->Tile


editRandomTile::Vector2->Vector2->TileModifier->TileBuilder
editRandomTile rangeX rangeY tileFunc _ position tile gen = if (isPosition requiredPosition position) then (tileFunc tile)
    else tile
    where {
        requiredPosition = Position randomX randomY;
        gen2 = snd (splitStdGen gen);
        randomX = fst (getRandomValue (fst rangeX) (snd rangeX) gen);
        randomY = fst (getRandomValue (fst rangeY) (snd rangeY) gen2);
        }




editTile::TileModifier->Position->TileBuilder
editTile tileFunc requiredPosition _ position tile gen = if (isPosition requiredPosition position) then (tileFunc tile)
    else tile




