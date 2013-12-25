-- Project Euler - Problem 15
-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List


instance Num a => Num [a] where
    (f:fs) + (g:gs) = f+g : fs+gs
    fs + []         = fs
    [] + gs         = gs
    (f:fs) * (g:gs) = f*g : [f]*gs + fs*(g:gs)
    _ * _           = []
    abs             = undefined
    signum          = map signum
    fromInteger n   = [fromInteger n]
    negate          = map (\x -> -x)


pascalTriangle = map ([1, 1] ^) [0..]


main = print . last . sort $ pascalTriangle !! 40
