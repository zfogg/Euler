import Data.List
import Data.Maybe
import qualified Data.Set as Set

primes3to4 = takeWhile (<10^4) (dropWhile (<10^3) primes)
cleanPotentials = setNub $ sort $ map sort $ filter (all (>10^3)) $ potentialMutations
potentialMutations = map primePermutations $ filter (atLeastPrimePerms 3) primes3to4

atLeastPrimePerms n = (>=n) . length . primePermutations
permutateDigits = setNub . map digitsToIntegral . permutations . intToDigits
primePermutations = filter (`elem` primes3to4) . permutateDigits

validation n = find (isValid n) . permutations
    where isValid n (x:_:z:_) = x - z == (-(n*2))

-- To prove:
proof = catMaybes $ (setNub . sort) $ map (validation 3330) cleanPotentials
main = do print proof

intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits n = intToDigits (n `div` 10) ++ [n `mod` 10]

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral (x:[]) = x
digitsToIntegral (x:xs) = x * (10 ^ length xs) + digitsToIntegral xs

setNub xs = Set.toList $ Set.fromList xs

primes :: [Int]
primes = 2:3:5: filter fastIsPrime (candidates 7 11)
    where candidates x y = x:y: candidates (x+6) (y+6)
          fastIsPrime n = not $ any isFactor [3, 5 .. intRoot n]
              where isFactor f = n `mod` f == 0
                    intRoot    = floor . sqrt . fromIntegral
