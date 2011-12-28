module Main where

import Data.List
import Control.Parallel.Strategies
import Euler

sumDiagonals n = sum $ (parMap rpar) (sum . diagonals) [0 .. nthOfSideLength]
    where diagonals x     = everyNth (x*2) (nthSpiral x)
          nthOfSideLength = (n+2) `div` 2 - 3

nthSpiral 0 = [1]
nthSpiral n = [
                  1 + last (nthSpiral (n - 1))
                  ..
                  1 + 8*(triangleNumbers !! (n - 1))
              ]

everyNth n xs = case drop (n-1) xs of
                 (y:ys) -> y : everyNth n ys
                 []     -> []

main = do print $ sumDiagonals 1001
