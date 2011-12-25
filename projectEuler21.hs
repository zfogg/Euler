module Main where

import Data.List
import Data.Maybe
import Control.Parallel.Strategies
import Control.Parallel

amicablePairs = [ (x, y) | (x, y) <- uniquePairs [1..10^4], (sigma x) - x == y && (sigma y) - y == x ]

-- Euler's Sigmas.
-- When n is prime, these functions are equivilent to each other.
nSigma m n p = map ((-) 1) [sigma m, sigma n, sigma p]

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

uniquePairs xs = concat $ zipWith (map . (,)) xs (tail $ tails xs)
-- And you can check large lists with this.
allTrue = all $ all (== True)
-- Example:
-- > allTrue $ take 100 sigmaProof

-- Et cetera.

-- Parallel factors.
factors   n = 1 : (factorsOf n) ++ [n]
factorsOf n = let candidates = [2..n `div` 4]
              in catMaybes $ map findMod candidates
                  where findMod x = if n `mod` x == 0 then Just x else Nothing

paraSum = (parMap rseq) (sum . factors)

primes :: Integral a => [a]
primes = 2:3:5:(filter fastIsPrime $ candidates 7 11)
    where candidates x y = x:y: candidates (x+6) (y+6)
          fastIsPrime n = not $ any isFactor [3, 5 .. intRoot n]
              where isFactor f = n `mod` f == 0
                    intRoot    = floor . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n < 2                      = False
    | n < 8 && (n == 2 || odd n) = True
    | even n || isFactor 5       = False
    | otherwise                  = not $ any isFactor [3, 5 .. root]
    where
        isFactor f = n `mod` f == 0
        root       = (floor . sqrt . fromIntegral) n

main = do print amicablePairs
