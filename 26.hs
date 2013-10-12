import Data.Ratio
import Data.List
import Data.Numbers.Primes
import Data.Function
import Control.Parallel.Strategies


main = print $ solution [2..(10^3)-1]

solution = maximumBy (compare `on` periodLength) . solutions
    where periodLength = length . \(_, _, c) -> c
          solutions    = (parMap rdeepseq) repeatingDigits . map (1%)

digits :: Rational -> [Int]
digits 0 = []
digits x = d : digits (10*x - fromIntegral d)
    where d = floor (10*x)

-- :: (denominator, non-repeating_digits, repeating_digits)
repeatingDigits :: Rational -> (Integer, [Int], [Int])
repeatingDigits n
    | finite n  = (d, ds, [])
    | otherwise = (d, prePeriod, period)
    where (d, ds)   = (denominator n, digits n)
          prePeriod = let pf = primeFactors d in take (max (count 2 pf) (count 5 pf)) ds
          period    = head $ filter (`divides` phiDigits) periods
          periods   = [ take' (f*x) phiDigits | x <- sort . factors $ phi d, f <- [1..] ]
          phiDigits = take' (phi d) $ drop (length prePeriod) ds

divides :: (Eq a) => [a] -> [a] -> Bool
divides [] ys = False
divides xs [] = False
divides xs ys = 0 == xs `modLength` ys && ys == take (length ys) (cycle xs)
    where modLength xs ys = length ys `mod` length xs

finite :: Rational -> Bool
finite x =  length pf == (count 2 pf) + (count 5 pf)
    where pf = primeFactors (denominator x)

primePowerFactors :: Integer -> [(Integer, Int)]
primePowerFactors x = [ (p, count p pf) | p <- nub pf ]
    where pf = primeFactors x

-- Euler's totient function.
phi :: Integer -> Integer
phi = product . map f . primePowerFactors
    where f (p, a) = p^(a-1) * (p-1)

factors :: Integer -> [Integer]
factors = nub .  map product .
          powerSet . ppfToList .  primePowerFactors
    where powerSet []     = []
          powerSet (x:xs) = xs : map (x:) (powerSet xs) ++ powerSet xs
          ppfToList       = concatMap (\(x,y) -> replicate y x)

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

-- So that I don't have to call fromIntegral.
take' = genericTake
