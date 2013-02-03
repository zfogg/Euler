import Data.Ratio


fractions = map (tupleLift intToTuple) (pairs [1..99])

cancelDigits ((n1,n2),(d1,d2))
    | n1 == d2 && eq = cancelled
    | n2 == d1 && eq = cancelled
    | otherwise      = arg
    where arg = ((n1,n2),(d1,d2))
          eq  = tupleToRatio arg == tupleToRatio cancelled
          cancelled
              | n1 == d2 && d1 /= 0 = ((0,n2),(0,d1))
              | n2 == d1 && d2 /= 0 = ((0,n1),(0,d2))
              | otherwise           = arg


pairs xs = [ (x, y) | (x, i) <- zip (init xs) [1..], y <- drop i xs ]

tupleLift f (x, y) = (f x, f y)

intToTuple x = (x `div` 10, x `mod` 10)

tupleToRatio ((n1,n2),(d1,d2)) = (10*n1 + n2) % (10*d1 + d2)


main = do
    let cancelledFractions = filter (\x -> x /= cancelDigits x) fractions
    let cancelledRatios    = map (tupleToRatio . cancelDigits) cancelledFractions
    print $ product cancelledRatios
