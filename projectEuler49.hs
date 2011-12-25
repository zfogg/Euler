import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- Main data flow.
primesToCheck         = listRange (<10^3) (<10^4) primes
sequenceCandidates    = filter ((3<=) . length . primePermutations) primesToCheck
candidatePermutations = map primePermutations sequenceCandidates
filteredCandidates    = filter (all (>10^3)) candidatePermutations
reducedCandidates     = setNub $ sort $ map sort filteredCandidates
proofCandidates       = map (validation 3330) reducedCandidates

primePermutations = filter (`elem` primesToCheck) . permutateDigits
    where permutateDigits = setNub . map digitsToIntegral . permutations . intToDigits

validation n = find (isValid n) . permutations
    where isValid n (x:_:z:_) = x - z == (-(n*2))

-- To prove:
proof = catMaybes $ (setNub . sort) proofCandidates
main = do print proof

-- Helper functions.

intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits n = intToDigits (n `div` 10) ++ [n `mod` 10]

listRange start stop = takeWhile stop . dropWhile start

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
