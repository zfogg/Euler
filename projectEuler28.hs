module Main where

import Data.List
import Control.Parallel.Strategies
import Euler

sumDiagonals n = sum $ 1 : parallelMap (sum . diagonals) [1 .. nthOfSideLength]
    where diagonals x     = everyNth (x*2) (nthSpiral x)
          nthOfSideLength = (n+2) `div` 2 - 1
          parallelMap     = parMap rpar

nthSpiral 0 = [1]
nthSpiral n = [ start .. end ]
    where start = 1 + last (nthSpiral (n - 1))
          end   = 1 + 8*triangleNumbers !! (n - 1)

everyNth n = map snd . filter ((== 0) . (`rem` n) . fst) . zip [1 .. ]

main = do print $ sumDiagonals 1001
