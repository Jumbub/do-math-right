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
import Data.List

import Type
import Utility

stringToOperand :: String -> Maybe Operand
stringToOperand string
    | isJust number = Just $ Fraction (fromJust number, 1, Exact)
    | isVariable = Just $ Variable (head string)
    | otherwise = Nothing
    where
        number = readMaybe string :: Maybe Integer
        isVariable = length string == 1 && isLower (head string)

operandToString :: Context -> Operand -> String
operandToString ctx (Variable var) = [var]
operandToString ctx (Expression ([Variable b, Variable a], Multiplication)) = [b] ++ [a]
operandToString Context {accuracy=Exact} (Fraction (num, den, precision))
    | num == den || den == 1 = show num
    | otherwise = show num ++ "/" ++ show den
operandToString Context {accuracy=PlusOrMinus dpAccuracy} (Fraction (num, den, precision))
    = decimalToString $ fractionToDecimal (decimalPlaces dpAccuracy) num den

fractionToDecimal :: Integer -> Integer -> Integer -> (Integer, [Integer])
fractionToDecimal _ _ 0 = error "Cannot divide by 0!"
fractionToDecimal acc num den = (div num den, (fractionToDecimal' acc den (rem num den) []))

fractionToDecimal' :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
fractionToDecimal' precision divisor remainder decimals
    | maxPrecision = []
    | noRemainder = decimals
    | otherwise = fractionToDecimal' precision divisor (rem numberToDivide divisor) (decimals ++ [div numberToDivide divisor])
    where
        noRemainder = remainder == 0
        maxPrecision = (genericLength decimals) == precision
        numberToDivide = 10 * remainder

decimalToString :: (Integer, [Integer]) -> String
decimalToString (whole, []) = show whole
decimalToString (whole, decimals) = (show whole) ++ "." ++ (concat $ map show decimals)

num :: Integer -> Operand
num number = Fraction (number, 1, Exact)

frac :: Integer -> Integer -> Operand
frac num den = Fraction (num, den, Exact)

var :: Char -> Operand
var letter = Variable letter
