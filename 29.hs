import Data.List
import Euler


main =
    print $ (length . nub) numbers
    where numbers =  [ x^y | x <- [2..100], y <- [2..100] ]
