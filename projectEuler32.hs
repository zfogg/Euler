import Euler
import Data.List (delete, foldl1)
import Data.Set (fromList, toList)

arePandigital xs
    | length xs == 9 = pandigital xs [1 .. 9]
    | otherwise      = False
    where pandigital []     ys = False
          pandigital (x:[]) ys = x `elem` ys
          pandigital (x:xs) ys = x `elem` ys && pandigital xs (delete x ys)

panProducts = [ x*y | x <- [1..10^4], y <- [1.. 10^3], valid x y ]
    where valid x y = arePandigital $ foldl1 (++) digitList
              where digitList = map intToDigits [x, y, x*y]

setNub = toList . fromList

main = do
    putStr "\n\tResults: "
    print $ (sum . setNub) panProducts
    putStrLn "\n"
