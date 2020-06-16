module Operand (
    Operand,
    stringToOperand,
    num,
    var,
) where

import Data.Maybe
import Data.Char
import Text.Read

type Operand = ((Int, Int), ([Char], [Char]))

stringToOperand :: String -> Maybe Operand
stringToOperand string
    | isJust number = Just (((fromJust number), 1), ([], []))
    | isVariable = Just ((1, 1), ([head string], []))
    | otherwise = Nothing
    where
        number = readMaybe string :: Maybe Int
        isVariable = length string == 1 && isLower (head string)


lazyIsNumber :: String -> Bool
lazyIsNumber input = isDigit $ head input

num :: Int -> Operand
num number = ((number, 1), ([], []))

var :: Char -> Operand
var variable = ((1, 1), ([variable], []))
