module Operand (
    Operand,
    stringToOperand,
    operandToString,
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

operandToString :: Operand -> String
operandToString ((num, den), (varNum, varDen))
    | num == den = show 1
    | den == 1 = show num
    | rem num den == 0 = show $ div num den

num :: Int -> Operand
num number = ((number, 1), ([], []))

var :: Char -> Operand
var variable = ((1, 1), ([variable], []))

