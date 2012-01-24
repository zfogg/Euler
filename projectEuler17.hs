import Euler
import Data.List
import Data.Char

numberString n =
    let d = intToDigits n in
        case (length d, d) of
            (0, _)       -> ""
            (1, (x:_))   -> ones n
            (2, (x:y:_)) -> if n > 10 && n < 20 then teens y
                            else tens x ++ if (== 0) y then []
                                           else "-" ++ ones y
            (3, (x:xs))  -> ones x ++ " hundred" ++ if all (== 0) xs then []
                                                    else " and " ++ numberString (digitsToIntegral xs)
            (4, (x:xs))  -> ones x ++ " thousand " ++ numberString (digitsToIntegral xs)

main = do
    let numStrs = map numberString [1 .. 1000]
        letters = map (filter isAlpha) numStrs
        letterCount = sum $ map length letters

    -- For debugging purposes:
    --putStrLn "Enter a number: "
    --n <- getLine
    --putStrLn (numberString $ stringToNum n)

    print $ filter isAlpha (numStrs !! ( 342 - 1 ))
    --print numStrs
    --print letters
    print letterCount

stringToNum = digitsToIntegral . map digitToInt

ones n = case n of
    0 -> ""
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
teens n = case n of
    0 -> ""
    1 -> "eleven"
    2 -> "twelve"
    3 -> "thirteen"
    4 -> "fourteen"
    5 -> "fifteen"
    6 -> "sixteen"
    7 -> "seventeen"
    8 -> "eighteen"
    9 -> "nineteen"
tens n = case n of
    0 -> ""
    1 -> "ten"
    2 -> "twenty"
    3 -> "thirty"
    4 -> "forty"
    5 -> "fifty"
    6 -> "sixty"
    7 -> "seventy"
    8 -> "eighty"
    9 -> "ninety"
