import Euler


endsAt89 1  = False
endsAt89 89 = True
endsAt89 x  = endsAt89 (sumOfSquaredDigits x)
    where sumOfSquaredDigits = (foldl  ((+) . (^) 2) 0) . intToDigits


main = do print $ length (filter endsAt89 [1..(10^7)-1])
