import Data.List (delete)
import Data.Set (fromList, toList, fromList)

intToDigits :: (Integral a) => a -> [a]
intToDigits 0 = []
intToDigits n = intToDigits (n `div` 10) ++ [n `mod` 10]

arePandigital xs =
    if length xs == 9
        then pandigital xs [1 .. 9]
        else False
    where
        pandigital []     ys = False
        pandigital (x:[]) ys = x `elem` ys
        pandigital (x:xs) ys = x `elem` ys && pandigital xs (delete x ys)

panProducts = [ x*y | x <- [1..10^4], y <- [1.. 10^3], valid x y ]
    where valid x y = arePandigital $ xDigits++yDigits++xyDigits
              where (xDigits, yDigits, xyDigits) = (intToDigits x, intToDigits y, intToDigits (x*y))

setNub = toList . fromList

main = do
    putStr "\n\tResults: "
    putStr $ show $ (sum . setNub) panProducts
    putStrLn "\n"
