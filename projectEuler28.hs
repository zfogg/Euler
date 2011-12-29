module Main where

import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import Euler

sumSpirals spirals n = sum $ spirals (nthSquare n)
    where nthSquare n = (n+2) `div` 2 - 1

-- Method #1.
spiralDiagonals1 n = 1 : parallelMap (sum . diagonals) [1 .. n]
    where diagonals x = everyNth (x*2) (nthSpiral x)

-- Method #2. Quicker and more parallel than Method #1.
spiralDiagonals2 n = 1 : map sum diagonals
    where diagonals = zipWith ($) everyDoubles spirals
              where spirals      = parallelMap nthSpiral [1 .. n]
                    everyDoubles = parallelMap everyDouble [1 .. n]
                        where everyDouble = everyNth . (2*)

nthSpiral :: Int -> [Integer]
nthSpiral 0 = [1]
nthSpiral n = [start .. end]
    where start = 1 + last (nthSpiral (n - 1))
          end   = 1 + 8*triangleNumbers !! (n - 1)

-- Helper functions

everyNth n = map snd . filter ((== n) . fst) . zip (cycle [1 .. n])

parallelMap = parMap rpar

-- Depracated, but still pretty cool.
pZipWith f [] _ = []
pZipWith f _ [] = []
pZipWith f (x:xs) (y:ys) = recur `par` f x y `pseq` f x y : recur
    where recur = pZipWith f xs ys

main = do
    --print $ sumSpirals spiralDiagonals1 1001
    print $ sumSpirals spiralDiagonals2 1001
