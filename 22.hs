import Data.List (sort, zipWith)
import Data.Char (isAlpha, ord)
import Data.List.Split (splitOn)


main = do
    namesStr <- readFile "lib/projectEuler22.txt"
    let names = sort . map (filter isAlpha) $ splitOn "," namesStr
        nameVals = map (sum . map alphaValue) names
        valsXIndices = zipWith (*) nameVals [1..]
    print $ sum valsXIndices

alphaValue c = ord c - 64
