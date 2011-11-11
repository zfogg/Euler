-- Project Euler - Problem 15
-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List

-- The following bits were never finished.
gridSections :: Int -> [Int]
gridSections n = [ x | x <- hs++[n `div` 2 + 1]++(reverse hs) ]
    where hs = [1 .. n `div` 2]

grid n sectionCounts = [ take (sectionCounts !! (x-1)) $ repeat x | x <- [1..n+1] ]

newGrid n = let n2 = n^2 in grid n2 (gridSections n2)

gridRoutes (g:[])   = []
gridRoutes (g:r:[]) = [routes g r]
gridRoutes (g:rid)  = [routes g (head rid)] ++ (gridRoutes rid)
routes xs ys = [ (x, y) | x <- xs, y <- ys, x < y ]


-- Forget that. Let's use Pascal's Triangle. It works.

memoizeWith :: (a -> a -> a) -> [a] -> [a] -> [a]
memoizeWith f xs []         = xs
memoizeWith f [] ys         = ys
memoizeWith f (x:xs) (y:ys) = f x y : memoizeWith f xs ys

extendWith f [] = []
extendWith f xs@(x:ys) = x : memoizeWith f xs ys

pascal = iterate (extendWith (+)) [1]

-- The answer is the biggest number in the 40th row of Pascal's Triangle.
-- The triangle is zero indexed.

main = do
    print $ (last . sort) $ pascal !! 40
