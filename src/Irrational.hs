module Irrational (
    Irrational(..),
    rationalise,
) where

import Data.Number.CReal
import Data.Char

import IrrationalType
import Fraction

rationalise :: Integer -> Irrational -> Fraction
rationalise dp Pi = ((piNumber, (10 ^ (dp + 1))), accuracy)
    where
        piString = "3" ++ (drop 2 $ showCReal (fromIntegral dp) pi) ++ "5"
        piNumber = stringToIntegral piString
        accuracy = (1, (10 ^ dp) * 2)

stringToIntegral :: String -> Integer
stringToIntegral [] = 0
stringToIntegral (d:rest) = (charToIntegral d) * 10 ^ length rest + stringToIntegral rest

charToIntegral :: Char -> Integer
charToIntegral c = fromIntegral $ digitToInt c
