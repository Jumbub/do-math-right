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
    | otherwise = decimalToString $ fractionToDecimal num den 10

decimalToString :: (Int, [Int]) -> String
decimalToString (whole, []) = show whole
decimalToString (whole, decimals) = (show whole) ++ "." ++ (concat (map (show) decimals))

fractionToDecimal :: Int -> Int -> Int -> (Int, [Int])
fractionToDecimal num 0 precision = error "Cannot divide by 0!"
fractionToDecimal num den precision = (div num den, fractionToDecimal' precision den (rem num den) [])

fractionToDecimal' :: Int -> Int -> Int -> [Int] -> [Int]
fractionToDecimal' precision divisor remainder decimals
    | noRemainder || maxPrecision = decimals
    | otherwise = fractionToDecimal' precision divisor (rem numberToDivide divisor) (decimals ++ [div numberToDivide divisor])
    where
        noRemainder = remainder == 0
        maxPrecision = length decimals == precision
        numberToDivide = 10 * remainder

num :: Int -> Operand
num number = ((number, 1), ([], []))

var :: Char -> Operand
var variable = ((1, 1), ([variable], []))

