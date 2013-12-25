-- Project Euler - Problem 15
-- Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
-- How many routes are there through a 20x20 grid?

import Data.List


instance Num a => Num [a] where
    (x:xs) + (y:ys) = x+y : xs+ys
    xs + []         = xs
    [] + ys         = ys
    (x:xs) * (y:ys) = x*y : [x]*ys + xs*(y:ys)
    _ * _           = []
    abs             = undefined
    signum          = map signum
    fromInteger n   = [fromInteger n]
    negate          = map negate


pascalsTriangle = map ([1, 1] ^) [0..]

solution = last . sort . (pascalsTriangle !!)


main = print $ solution 40
