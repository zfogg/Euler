-- Project Euler - Problem 15
-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List

memoizeWith :: (a -> a -> a) -> [a] -> [a] -> [a]
memoizeWith f xs []         = xs
memoizeWith f [] ys         = ys
memoizeWith f (x:xs) (y:ys) = f x y : memoizeWith f xs ys

extendWith f [] = []
extendWith f xs@(x:ys) = x : memoizeWith f xs ys

pascal = iterate (extendWith (+)) [1]

-- The answer is the biggest number in the 40th row of Pascal's Triangle.
-- The triangle is zero indexed.

main = print $ (last . sort) $ pascal !! 40
