module Main where

import Data.List
import Data.Maybe
import Control.Parallel.Strategies
import Control.Parallel
import Euler

amicablePairs = [ (x, y) | (x, y) <- uniquePairs [1..10^4], (sigma x) - x == y && (sigma y) - y == x ]

-- Euler's Sigmas.
-- When n is prime, these functions are equivilent to each other.
nSigma m n p = parMapStrat ((-) 1) [sigma m, sigma n, sigma p]

sigma1 (n, k) = sum $ factors (n^k)
sigma2 (n, k) = (n^(k+1)-1) `div` (n-1)
sigma3 (n, k) = (+1) $ sum [ n^x | x <- [1..k] ]
sigma4 n      = sum $ factors n
sigma  n      = sigma4 n

-- And here is a list of boolean results, as proof.
-- This proof should return lists of True values.
sigmaProof = [ checks [sigma1, sigma2, sigma3] (n, k) | k <- [2..10], n <- primes ]
    where checks xs nK = [ check fs nK | fs <- uniquePairs xs ]
          check (f1, f2) nK = (f1 nK) == (f2 nK)

uniquePairs xs = concat $ zipWith (parMapStrat . (,)) xs (tail $ tails xs)
-- And you can check large lists with this.
allTrue = all $ all (== True)
-- Example:
-- > allTrue $ take 100 sigmaProof

-- Et cetera.

-- Parallel factors.
factors   n = 1 : (Main.factorsOf n) ++ [n]
factorsOf n = let candidates = [2..n `div` 4]
              in catMaybes $ parMapStrat findMod candidates
                  where findMod x = if n `rem` x == 0 then Just x else Nothing

parMapStrat = parMap rseq

main = do print amicablePairs
