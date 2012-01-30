import Euler
import Data.List
import Control.Parallel
import Control.Parallel.Strategies


sumSpirals :: (Num a, Integral b) => (b -> [a]) -> b -> a
sumSpirals spirals n = sum $ spirals (nthSquare n)
    where nthSquare x = (x + 2) `div` 2 - 1

-- Method #1.
spiralDiagonals1 :: Int -> [Integer]
spiralDiagonals1 n = 1 : map' (sum . diagonals) [1 .. n]
    where diagonals x = everyNth (x * 2) (nthSpiral x)

-- Method #2. Quicker and more parallel than Method #1.
spiralDiagonals2 :: Int -> [Integer]
spiralDiagonals2 n = 1 : map sum diagonals
    where diagonals = zipWith' ($) everyDoubles spirals
              where spirals      = map' nthSpiral [1 .. n]
                    everyDoubles = map (everyNth . (2 *)) [1 .. n]

nthSpiral :: Int -> [Integer]
nthSpiral 0 = [1]
nthSpiral n = [start .. end]
    where start = 1 + (last . nthSpiral) (n - 1)
          end   = 1 + 8 * triangleNumbers !! (n - 1)

-- Helper functions
everyNth :: (Num a, Enum a) => a -> [b] -> [b]
everyNth n = map snd . filter ((== n) . fst) . zip (cycle [1 .. n])

map' = parMap rpar

-- zipWith in parallel.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = p' `par` f' `pseq` f' : p'
    where p' = zipWith' f xs ys
          f' = f x y

main = do
    --print $ sumSpirals spiralDiagonals1 1001
    print $ sumSpirals spiralDiagonals2 1001
