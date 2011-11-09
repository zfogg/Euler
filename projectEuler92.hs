intToDigits :: (Integral a) => a -> [a]
intToDigits 0 = []
intToDigits n = intToDigits (n `div` 10) ++ [n `mod` 10]

sumOfSquaredDigits = (foldl (\acc x -> acc + (x^2)) 0) . intToDigits

endsAt89 1  = False
endsAt89 89 = True
endsAt89 x  = endsAt89 (sumOfSquaredDigits x)


main = do print $ length (filter endsAt89 [1..(10^7)-1])
