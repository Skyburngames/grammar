{-
    - Random based helper functions
-}

module RandomUtils
(
    createRandomGen,
    getRandomValue,
    splitStdGen,
    randomRoll,
    createGenerators,
    randomPosition
) where

import System.Random
import Grammar


-- =============================================== Randomness ====================================================== --
splitStdGen::StdGen->(StdGen,StdGen)
splitStdGen gen = split gen

createRandomGen::StdGen
createRandomGen = mkStdGen 5454222

getRandomValue::Int->Int->StdGen->(Int, StdGen)
getRandomValue min max gen = randomR (min, max) gen

getRandomFloatValue::Float->Float->StdGen->(Float, StdGen)
getRandomFloatValue min max gen = randomR (min, max) gen

createGenerators::Int->StdGen->[StdGen]
createGenerators 0 _ = []
createGenerators remaining startGen = startGen:(createGenerators (remaining-1) (snd (split startGen)))

randomRoll::Float->StdGen->Bool
randomRoll chance gen = (chance >= (fst (getRandomFloatValue 0 1 gen)))

randomPosition::Vector2->Vector2->StdGen->Position
randomPosition rangeX rangeY gen = Position randomX randomY
  where{
    genX = gen;
    genY = snd (splitStdGen gen);
    randomX = fst (getRandomValue (fst rangeX) (snd rangeX) genX);
    randomY = fst (getRandomValue (fst rangeY) (snd rangeY) genY);
}
