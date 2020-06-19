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
operandToString ((num, den), (numVarsUnsorted, denVarsUnsorted))
    | noVariables && isDecimal = decimalString
    | noVariables = show num ++ "/" ++ show den
    | null denVars && isDecimal && isOne = numVars
    | null denVars && isDecimal = decimalString ++ numVars
    | num == 1 && den == 1 && null numVars = "1/" ++ denVars
    | num == 1 && den == 1 = numVars ++ "/" ++ denVars
    | num == 1 = numVars ++ "/" ++ show den ++ denVars
    | den == 1 = show num ++ numVars ++ "/" ++ denVars
    | otherwise = show num ++ numVars ++ "/" ++ show den ++ denVars
    where
        numVars = sort numVarsUnsorted
        denVars = sort denVarsUnsorted
        noVariables = null numVars && null denVars
        isDecimal = isJust decimal
        isFraction = isNothing decimal
        decimal = fractionToDecimal num den 10
        isOne = num == den && num == 1
        decimalString = decimalToString (fromJust decimal)

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
num number = ((number, 1), ([], []))

frac :: Int -> Int -> Operand
frac num den = ((num, den), ([], []))

var :: Char -> Operand
var variable = ((1, 1), ([variable], []))
