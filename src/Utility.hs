module Utility (
    uniqueValues,
    gcd,
    decimalPlaces,
) where

uniqueValues :: [Char] -> [Char]
uniqueValues input = uniqueValues' input []

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

decimalPlaces :: (Int, Int) -> Int
decimalPlaces (num, den) = digits den - digits num

digits :: Int -> Int
digits x = round $ logBase 10.0 (fromIntegral x)
