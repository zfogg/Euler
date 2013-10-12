module Euler where

import qualified Data.List as List
import qualified Data.Function as Function
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set


hypotenuse :: (Integral a, Floating b) => a -> a -> b
hypotenuse a b = sqrt $ fromIntegral (a^2 + b^2)

factorial' :: (Integral a) => a -> a
factorial' 1 = 1
factorial' n = n * factorial' (n-1)

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n < 2                      = False
    | n < 8 && (n == 2 || odd n) = True
    | even n || isFactor 5       = False
    | otherwise                  = not $ any isFactor [3, 5 .. root]
    where
        isFactor f = n `rem` f == 0
        root       = (floor . sqrt . fromIntegral) n

rotateList :: (Integral a) => a -> [b] -> [b]
rotateList 0 xs     = xs
rotateList n (x:xs) = rotateList (n-1) (xs++[x])

intToDigits :: (Integral a) => a -> [a]
intToDigits 0 = []
intToDigits n = intToDigits (n `div` 10) ++ [n `mod` 10]

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral (x:[]) = x
digitsToIntegral (x:xs) = x * (10 ^ length xs) + digitsToIntegral xs

divisors :: (Integral a) => a -> [a]
divisors n = filter ((== 0) . rem n) [2..n `div` 2]

arePalindromic :: (Eq a) => [a] -> Bool
arePalindromic []     = True
arePalindromic (x:[]) = True
arePalindromic (x:xs) = x == last xs && arePalindromic (init xs)

isPalindrome :: (Integral a) => a -> Bool
isPalindrome = arePalindromic . intToDigits

intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' (x:[]) ys = [ x | x `elem` ys ]
intersect' (x:xs) ys
    | x `elem` ys = x : intersect' xs ys
    | otherwise   = intersect' xs ys

--- ProjectEuler helper functions.
truncations x = map digitsToIntegral truncts
    where truncts = tail (List.inits xs) ++ (init . tail) (List.tails xs)
          xs = intToDigits x

digitCount = length . intToDigits

transformDigits f = digitsToIntegral . f . intToDigits

firstDigit i = if i >= 10 then firstDigit (i `div` 10) else i
lastDigit  i = i `mod` 10

--
-- Number Sets
--

fibonacci = 1:1:[ a + b | (a, b) <- zip fibonacci (tail fibonacci) ]

candidates' x y = x:y: candidates' (x+6) (y+6)

primes = 2:3:5 : filter fastIsPrime (candidates 7 11)
    where candidates x y = x:y: candidates (x+6) (y+6)
          fastIsPrime n = not $ any isFactor [3, 5 .. sqrtOf n]
              where isFactor f = n `rem` f == 0
                    sqrtOf     = floor . sqrt . fromIntegral

circlePrimes = filter (all isPrime . circledDigits) primes
    where circledDigits x = [ rotateDigits times x | times <- [1 .. digitCount x] ]
          rotateDigits    = transformDigits . rotateList

pandigitals = filter isPandigital [1..]
isPandigital x = pandigital (intToDigits x) [1 .. digitCount x ]
    where pandigital []     ys = False
          pandigital (x:[]) ys = x `elem` ys
          pandigital (x:xs) ys = x `elem` ys && pandigital xs (List.delete x ys)

triangleNumbers   = map (\n -> n * (n-1) `div` 2)     [2 ..]
pentagonalNumbers = map (\n -> n * (3*n - 1) `div` 2) [1 ..]
hexagonalNumbers  = map (\n -> n * (2*n - 1))         [1 ..]
