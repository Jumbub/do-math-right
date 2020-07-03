module Utility (
    uniqueValues,
    gcd,
    decimalPlaces,
    addFraction,
    simplifyFraction,
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

decimalPlaces :: (Integer, Integer) -> Integer
decimalPlaces (num, den) = digits den - digits num

digits :: Integer -> Integer
digits x = 1 + (round $ logBase 10.0 (fromIntegral x))

isGreater :: (Integer, Integer) -> (Integer, Integer) -> Bool
isGreater (a, b) (x, y) = (a * y) > (x * b)

addFraction :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addFraction (a, b) (x, y) = (a * y + x * b, b * y)

simplifyFraction :: (Integer, Integer) -> (Integer, Integer)
simplifyFraction (num, den) = (numerator, denominator)
    where
        commonDen = gcd num den
        numerator = num `div` commonDen * flip
        denominator = den `div` commonDen * flip
        flip = if (num < 0 && den < 0) || (den < 0 && num >= 0) then -1 else 1
