module Utility (
    uniqueValues,
    gcd,
    decimalPlaces,
    removeTrailingZeros,
) where

uniqueValues :: [Char] -> [Char]
uniqueValues input = uniqueValues' input []
    where
        uniqueValues' :: [Char] -> [Char] -> [Char]
        uniqueValues' [] out = out
        uniqueValues' values unique
            | existsIn (head values) unique = uniqueValues' (tail values) unique
            | otherwise = uniqueValues' (tail values) (head values: unique)

existsIn :: Char -> [Char] -> Bool
existsIn _ [] = False
existsIn check array
    | check == (head array) = True
    | otherwise = existsIn check (tail array)

decimalPlaces :: (Integer, Integer) -> Integer
decimalPlaces (num, den) = digits den - digits num

digits :: Integer -> Integer
digits x = 1 + (round $ logBase 10.0 (fromIntegral x))

removeTrailingZeros :: String -> String
removeTrailingZeros input = reverse (removeTrailingZeros' (reverse input))
    where
        removeTrailingZeros' :: String -> String
        removeTrailingZeros' ('0' : rest) = removeTrailingZeros' rest
        removeTrailingZeros' rest = rest
