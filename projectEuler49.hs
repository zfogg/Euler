import Data.List
import Data.Maybe
import Control.Parallel.Strategies
import qualified Data.Set as Set
import Euler

-- Main data flow.
primesToCheck         = listRange (<10^3) (<10^4) primes
sequenceCandidates    = filter ((3<=) . length . primePermutations) primesToCheck
candidatePermutations = parallelMap primePermutations sequenceCandidates
filteredCandidates    = filter (all (>10^3)) candidatePermutations
reducedCandidates     = setNub $ sort $ parallelMap sort filteredCandidates
proofCandidates       = parallelMap (validation 3330) reducedCandidates

primePermutations = filter (`elem` primesToCheck) . permutateDigits
    where permutateDigits = setNub . map digitsToIntegral . permutations . intToDigits

validation n = find (isValid n) . permutations
    where isValid n (x:_:z:_) = x - z == (-(n*2))

-- To prove:
proof = catMaybes $ (setNub . sort) proofCandidates
main = do print proof

-- Helper functions.

listRange start stop = takeWhile stop . dropWhile start

setNub xs = Set.toList $ Set.fromList xs

parallelMap = parMap rpar
