import Euler
import Data.List (delete, foldl1)
import Data.Set (fromList, toList)

panProducts = [ x*y | x <- [1..10^4], y <- [1.. 10^3], valid x y ]
    where valid x y = isPandigital $ digitsToIntegral digitList
              where digitList = concatMap intToDigits [x, y, x*y]

setNub = toList . fromList

main = do
    putStr "\n\tResults: "
    print $ (sum . setNub) panProducts
    putStrLn "\n"
