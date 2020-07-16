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
import Data.List.Split

import OperandType
import Operator
import Context
import Utility
import Decimal
import ExactFraction

stringToOperand :: String -> Maybe Operand
stringToOperand input
    | isJust fraction = Just $ Fraction (exactFraction, (0, 1))
    | isVariable = Just $ Variable (head input)
    | otherwise = Nothing
    where
        exactFraction = ExactFraction.simplify $ fromJust fraction
        fraction = stringToFraction input
        isVariable = length input == 1 && isLower (head input)

stringToFraction :: String -> Maybe (Integer, Integer)
stringToFraction input
    | isNothing entireWholeInt = Nothing
    | otherwise = Just (numerator, denominator)
    where
        numerator = fromJust entireWholeInt
        denominator = 10 ^ (length cleanDecimals)
        entireWholeInt = readMaybe (whole ++ cleanDecimals) :: Maybe Integer
        cleanDecimals = if null decimals then "" else (removeTrailingZeros $ head decimals)
        (whole:decimals) = splitOn "." input

operandToString :: Context -> Operand -> String
operandToString ctx (Variable var) = [var]
operandToString ctx (Expression ([Variable b, Variable a], Multiplication)) = [b] ++ [a]
operandToString Context {fractionResult=True} (Fraction ((num, den), (0, _)))
    | num == den || den == 1 = show num
    | otherwise = show num ++ "/" ++ show den
operandToString Context {fractionResult=True} (Fraction ((num, den), (accNum, accDen)))
    | num == den || den == 1 = show num ++ innaccuracy
    | otherwise = show num ++ "/" ++ show den ++ innaccuracy
    where
        innaccuracy = " ± " ++ show accNum ++ "/" ++ show accDen
operandToString ctx (Fraction ((num, den), accuracy))
    = decimalToString $ fractionToDecimal ctx ((num, den), accuracy)

num :: Integer -> Operand
num number = Fraction ((number, 1), (0, 1))

frac :: Integer -> Integer -> Operand
frac num den = Fraction ((num, den), (0, 1))

var :: Char -> Operand
var letter = Variable letter
