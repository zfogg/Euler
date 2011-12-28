module Main where

import Data.List
import Control.Parallel
import Euler

amicablePairs = filter parIsAmic (uniquePairs [2..10^3])
    where parIsAmic (x, y) = (q `par` p) && (p `pseq` q)
             where amicable x y = sigma x - x == y
                   p = amicable x y
                   q = amicable y x

-- Euler's Sigmas.
nSigma m n p = map ((+) (-1)) [sigma m, sigma n, sigma p]

sigma n    = sum $ factors n

sigma1 k n = sum $ factors (n^k)
sigma2 k n = sum $ 1 : [ n^x | x <- [1..k] ]
sigma3 k n = (n^(k+1)-1) `div` (n-1)

-- And here is a list of boolean results, as proof.
-- This proof should return lists of True values.
sigmaProof = [ checks [sigma1, sigma2, sigma3] k n | k <- [2..10], n <- primes ]
    where checks xs k n = [ check fs k n | fs <- uniquePairs xs ]
              where check (f1, f2) k n = f1 k n == f2 k n

uniquePairs xs = concat $ zipWith (map . (,)) xs (tail $ tails xs)
-- And you can check large lists with this.
allTrue = all and
-- Example:
-- > allTrue $ take 100 sigmaProof

-- Et cetera.

factors n = 1 : divisors n ++ [n]
reFactors n = 1 : (divisors n) ++ [n]
    where f []     = []
          f (x:xs) = if n `rem` x == 0
                        then x : f xs
                        else     f xs

main = do print $ amicablePairs
