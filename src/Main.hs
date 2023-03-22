module Main where

import Control.Monad

import Data.List
import Data.Functor

import System.Random

type DNA = String

-- Variables

targetDNA :: DNA
targetDNA = "Haskell"

mutationChance :: Double
mutationChance = 0.1

populationSize :: Int
populationSize = 500

numRounds :: Int
numRounds = 10000

-- Functions

clamp :: Ord a => (a, a) -> a -> a
clamp (lower, upper) value
  | value < lower = lower
  | value > upper = upper
  | otherwise = value

scoreDNA :: DNA -> Double
scoreDNA dna = foldl' (+) 0 $ zipWith (\a b -> if a == b then 1 else 0) dna targetDNA
--scoreDNA dna = foldl' (+) 0 $ zipWith compareChar dna targetDNA
--  where compareChar a b
--          | a == b = 1
--          | otherwise = abs $ fromIntegral (fromEnum a - fromEnum b) / 255

sortDNA :: [DNA] -> [DNA]
sortDNA = sortOn (negate . scoreDNA)

mutateChar :: Char -> IO Char
mutateChar c = do
  r <- randomRIO (0.0, 1.0)
  up <- randomRIO (True, False)

  let shift = if up then 1 else (-1)

  if r > mutationChance then return c
    else return $ toEnum $ clamp (32, 126) $ fromEnum c + shift

mutateDNA :: DNA -> IO DNA
mutateDNA = mapM mutateChar

crossDNA :: DNA -> DNA -> IO (DNA, DNA)
crossDNA a b = do
  let minLength = min (length a) (length b)

  r <- randomRIO (0, minLength)

  let (a1, a2) = splitAt r a
      (b2, b1) = splitAt r b

  return (a1 ++ b1, a2 ++ b2)

genRandomDNA :: IO DNA
genRandomDNA = replicateM (length targetDNA) $ randomRIO (32, 126) <&> toEnum

genPopulation :: IO [DNA]
genPopulation = replicateM populationSize genRandomDNA

-- Modified Code from https://mihai.page/evolving-is-digit/
selectFromPopulation :: Double -> [(DNA, Double)] -> IO DNA
selectFromPopulation total scoredDNA = do
  dart <- randomRIO (0, total - 1) :: IO Double
  return $ go dart scoredDNA
    where go _ [] = []
          go x ((d,s):ds)
            | s > x = go x ds
            | otherwise = d

-- Int: Number of Rounds
-- Modified Code from https://mihai.page/evolving-is-digit/
evoRounds :: Int -> [DNA] -> IO DNA
evoRounds _ [] = return "" -- This line should never run if used properly
evoRounds 0 (d:_) = return d
evoRounds n (d:ds) = do
  let fitness = map scoreDNA ds
      summed = scanl1 (+) fitness
      roulette = zip ds summed
      total = last summed

  newPopulation <- replicateM (populationSize - 1) $ do
    a <- selectFromPopulation total roulette
    b <- selectFromPopulation total roulette
    (c1, c2) <- crossDNA a b

    mapM mutateDNA [c1, c2]

  evoRounds (n - 1) (take populationSize $ sortDNA $ d : concat newPopulation)

main :: IO ()
main = do
  -- Generate initial population
  population <- sortDNA <$> genPopulation
  best <- evoRounds numRounds population

  putStrLn $ "Best DNA: " ++ show best
  putStrLn $ "Score: " ++ show (scoreDNA best)
