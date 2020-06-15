module Operand (
    Operand,
    stringToOperand,
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
