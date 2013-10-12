import Euler
import Data.List
import Data.Function
import Control.Parallel

amicablePairs = filter parallelIsAmicable (uniquePairs [2..10^3])
    where parallelIsAmicable (x, y) = (q `par` p) && (p `pseq` q)
            where (p, q) = (amicable x y, amicable y x)
                  amicable x y = sigma x - x == y

-- Euler's Sigmas.
sigma x    = sum $ factors x

-- { s1 = s2 = s3   if x is prime
--   s(x, k) = x+k  if x is prime and k is 1 }
sigma1 x k = sum $ factors (x^k)
sigma2 x k = sum $ 1 : map (x^) [1..k]
sigma3 x k = (x^(k+1)-1) `div` (x-1)

-- And here is a list of boolean results, as proof.
-- This proof should return lists of True values.
sigmaProof = [ checks [sigma1, sigma2, sigma3] k n | k <- [2..10], n <- primes ]
    where checks xs k n = [ check fs k n | fs <- uniquePairs xs ]
            where check (f1, f2) k n = f1 k n == f2 k n

uniquePairs xs = [ (head x, y) | x <- (init $ tails xs), y <- (tail x) ]

-- And you can check large lists with this.
allTrue = all and
-- Example:
-- > allTrue $ take 100 sigmaProof

-- Et cetera.

factors x = 1 : divisors x ++ [x]

main = print amicablePairs
