module Main where

import Euler
import Data.List
import Data.Function
import Control.Parallel
import Control.Parallel.Strategies

sumDiagonals n = sum $ (parMap rpar) (sum . diagonals) [0 .. nthOfSideLength]
    where nthOfSideLength = (n+2) `div` 2 - 1
          diagonals x = everyNth (x*2) (nthSpiral x)

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
