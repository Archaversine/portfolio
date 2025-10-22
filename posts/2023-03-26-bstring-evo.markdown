---
title: Binary String Evolution in Haskell
author: Adam Brohl
---

## Problem 

The goal in this article is to find the largest number that can be represented
with 20 bits via evolutionary computation.

In order to solve this problem, there first needs to be a way to represent
each candidate. Instead of storing an array of 20 1s and 0s, the `Word32` type
from `Data.Word` can be used to store the relevant data. All calculations with
the `Word32` type will be made such that the other 12 bits will always be 0. 
This allows for the value of the `Word32`` to be the fitness function itself and
allows for the use of bit-wise operations to manipulate the 1s and 0s.

Before starting, here is a list of all imports and types that will be used for
the project:

```haskell 
import Data.Word
import Data.Bits
import Data.List
import Data.Functor ((<&>))

import Control.Monad
import Control.Monad.State

import System.Random

type Genome           = Word32
type Population       = [Genome]
type ScoredPopulation = [(Genome, Float)]
```

It will also be helpful to see the bits of a `Genome` as well, so here is a 
helper function to do just that:

```haskell 
printBits :: Genome -> IO ()
printBits w = do
    -- Loop backwards to print higher bits first
    mapM_ (\n -> putStr . show $ 1 .&. (w `shiftR` n)) [19,18..0] 
    putStrLn $ " = " ++ show w
```

## Genetic Algorithm Variables

For the genetic algorithm, a few helper variables to make everything easier
should be defined. Here are some that I thought were helpful in writing the code:

```haskell 
-- Number where only 20/32 bits are 1
-- Only Used to set max limit for random generation
mask :: Genome
mask = 1048575

population :: Int
population = 500

generations :: Int
generations = 500

iterations :: Int
iterations = 30

mutationChance :: Float
mutationChance = 0.001
```

## Initial Generation
Before any more functions are written, there first needs to be a way to both
randomly generate a genome, and randomly generate a population of genomes so
that an initial population for the genetic algorithm can be easily created.

```haskell 
-- Generate a single genome
genRandomGenome :: State StdGen Genome
genRandomGenome = randomR (0, mask)

-- Generate a list of genomes
genInitialPopulation :: State StdGen Population
genInitialPopulation = replicateM population genRandomGenome
```

## Crossover

The Crossover function is implemented using the standard crossover technique,
where both parents are split into two parts and merged to create a child.
Thanks to bit-wise operations, this process is not too difficult. Below is
the algorithm used to calculate the child’s DNA. Note that this algorithm
assumes the largest 12 bits of both parents are 0, which allows for no need to
modify the 12 bits after the crossover. Crossover happens always.

```haskell
cross :: Genome -> Genome -> State StdGen Genome
cross p1 p2 = do
    crossPoint <- randomR (0 :: Int, 20)
    let crossMask = complement 0 `shiftL` crossPoint

    return $ (p1 .&. crossMask) .|. (p2 .&. complement crossMask)
```

## Mutation

For each bit in the 20 bits of the `Word32`, there is a small chance (set to 0.1%
later on) that it will be flipped after crossover occurs. The bit is flipped
with bit-wise xor. Mutation always happens after crossover occurs. Here is a
function that loops through every bit with a small probability `mutationChance`
of mutating:

```haskell
mutate :: Genome -> State StdGen Genome
mutate g = go 20 (pure g)
    where go 0 acc = acc
          go n acc = do
            r <- randomR (0 :: Float, 1)
            if r > mutationChance then go (n - 1) acc
                -- flip current bit
                else go (n - 1) $ acc <&> (`xor` (1 `shiftL` (n - 1)))
```

## Population Selection

When parents are selected from the population, a roulette selection is used to
bias the selection towards candidates with a higher fitness. The process is
similar to a weighted random selection. See below:

```haskell
selectFromPopulation :: ScoredPopulation -> Float -> State StdGen Genome
selectFromPopulation [] _ = error "empty population"
selectFromPopulation scored totalFitness = do
    r <- randomRIO (0 :: Float, 1)
    return $ go (r * totalFitness) scored
        where go _ [] = undefined -- error on emtpy list
              go _ [(x, _)] = x
              go dart ((x, p):xs)
                | dart > p = go dart xs
                | otherwise = x
```

## Creating the Next Generation

To create the next generation, two parents are selected from the population,
crossed, and then mutated which yields the child for the next generation. Since
the crossover, mutation, and selection functions have already been defined,
this one is much more simple:

```haskell 
reproduce :: Population -> State StdGen Population
reproduce p = replicateM population $ do
    p1 <- selectFromPopulation scored totalFitness
    p2 <- selectFromPopulation scored totalFitness

    cross p1 p2 >>= mutate
        where summed = scanl1 (+) $ map fromIntegral p :: [Float]
              scored = zip p summed
              totalFitness = last summed
```

## The Loop 

Now, there needs to be a function that puts everything together. This function
should keep creating the next generation with crossover and mutation for a
given number of times. After that, it should return the genome with the best
fitness. The `generations` variable from earlier will be used to decide this
number.

```haskell 
runGA :: State StdGen Genome
runGA = go generations genInitialPopulation
    where go 0 pop = pop <&> head -- Return best fit genome
          go n pop = go (n - 1) $ pop >>= reproduce <&> sortBy (flip compare)
```

Note that `runGA` must sort the new population in descending order before making
the next recursive call. This is so that when the loop is done, `runGA` can
simply return the first element of the population, which if sorted in
descending order will be the best fit genome.

## Main Function

Finally, everything can be combined together into the main function. To make
things easier, the main function prints the variables used in the genetic
algorithm every time it is run. The main function also calls `runGA` multiple
times and then calculates the average and standard deviation of each best
performing genome.

```haskell 
main :: IO ()
main = do 
    putStrLn "--------------------------------------------"
    putStrLn $ "Population Size: " ++ show population
    putStrLn $ "Number of Generations: " ++ show generations
    putStrLn $ "Number of Iterations: " ++ show iterations
    putStrLn $ "Mutation per bit: " ++ show (mutationChance * 100) ++ "%"
    putStrLn "--------------------------------------------"

    results <- go iterations (pure [])

    let floats = map fromIntegral results :: [Float]
        mean = sum floats / fromIntegral iterations
        totalDiffSquared = sum $ map (\x -> (x - mean) ^ 2) floats
        deviation = sqrt $ totalDiffSquared / fromIntegral iterations

    putStrLn $ "Average: " ++ show mean
    putStrLn $ "Standard Deviation: " ++ show deviation

    where go 0 acc = acc
          go n acc = do
            best <- evalState runGA <$> newStdGen
            putStr ("Iteration " ++ show i ++ ": ") >> printBits best
            go (n - 1) $ (best :) <$> acc
                where i = iterations - n + 1
```

## Results

Here is the output generated from running the program one time, in iteration
24, it actually reaches the optimal solution of all 1s:

```
--------------------------------------------
Population Size: 500
Number of Generations: 500
Number of Iterations: 30
Mutation per bit: 0.1%
--------------------------------------------
Iteration 1: 11111111111111011011 = 1048539
Iteration 2: 11111111111111111010 = 1048570
Iteration 3: 11111111111111110001 = 1048561
Iteration 4: 11111111111111111011 = 1048571
Iteration 5: 11111111111111110100 = 1048564
Iteration 6: 11111111111110101000 = 1048488
Iteration 7: 11111111111101111000 = 1048440
Iteration 8: 11111111111110010111 = 1048471
Iteration 9: 11111111111111011110 = 1048542
Iteration 10: 11111111111111111010 = 1048570
Iteration 11: 11111111111111111101 = 1048573
Iteration 12: 11111111111001010111 = 1048151
Iteration 13: 11111111111110010111 = 1048471
Iteration 14: 11111111111110101111 = 1048495
Iteration 15: 11111111111111011011 = 1048539
Iteration 16: 11111111100100010010 = 1046802
Iteration 17: 11111111111110111111 = 1048511
Iteration 18: 11111111101111010100 = 1047508
Iteration 19: 11111111111101111110 = 1048446
Iteration 20: 11111111111111010110 = 1048534
Iteration 21: 11111111111111110001 = 1048561
Iteration 22: 11111111111111110100 = 1048564
Iteration 23: 11111111110101110011 = 1047923
Iteration 24: 11111111111111111111 = 1048575
Iteration 25: 11111111111001011010 = 1048154
Iteration 26: 11111111111011110011 = 1048307
Iteration 27: 11111111111111111110 = 1048574
Iteration 28: 11111111111110100010 = 1048482
Iteration 29: 11111111111011101010 = 1048298
Iteration 30: 11111111111111111000 = 1048568
Average: 1048378.4
Standard Deviation: 371.59454
```
