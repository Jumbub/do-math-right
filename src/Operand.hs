module Operand (
    Operand,
    stringToOperand,
    operandToString,
    num,
    var,
    frac,
) where

import Data.Maybe
import Data.Char
import Data.List
import Text.Read

import Type

stringToOperand :: String -> Maybe Operand
stringToOperand string
    | isJust number = Just $ Fraction (fromJust number, 1)
    | isVariable = Just $ Variable (head string)
    | otherwise = Nothing
    where
        number = readMaybe string :: Maybe Int
        isVariable = length string == 1 && isLower (head string)

operandToString :: Operand -> String
operandToString (Fraction (num, den))
    | isDecimal = decimalString
    | otherwise = show num ++ "/" ++ show den
    where
        isDecimal = isJust decimal
        isFraction = isNothing decimal
        decimal = fractionToDecimal num den 10
        isOne = num == den && num == 1
        decimalString = decimalToString (fromJust decimal)
operandToString (Variable var) = [var]
operandToString (Expression ([Variable b, Variable a], Multiplication)) = [b] ++ [a]

fractionToDecimal :: Int -> Int -> Int -> Maybe (Int, [Int])
fractionToDecimal num 0 precision = error "Cannot divide by 0!"
fractionToDecimal num den precision
    | isJust decimals = Just (div num den, (fromJust decimals))
    | otherwise = Nothing
    where
        decimals = fractionToDecimal' precision den (rem num den) []

fractionToDecimal' :: Int -> Int -> Int -> [Int] -> Maybe [Int]
fractionToDecimal' precision divisor remainder decimals
    | maxPrecision = Nothing
    | noRemainder = Just decimals
    | otherwise = fractionToDecimal' precision divisor (rem numberToDivide divisor) (decimals ++ [div numberToDivide divisor])
    where
        noRemainder = remainder == 0
        maxPrecision = length decimals == precision
        numberToDivide = 10 * remainder

decimalToString :: (Int, [Int]) -> String
decimalToString (whole, []) = show whole
decimalToString (whole, decimals) = (show whole) ++ "." ++ (concat $ map show decimals)

num :: Int -> Operand
num number = Fraction (number, 1)

frac :: Int -> Int -> Operand
frac num den = Fraction (num, den)

var :: Char -> Operand
var letter = Variable letter
