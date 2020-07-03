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
import Decimal

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
operandToString Context {fractionResult=True} (Fraction (num, den, Exact))
    | num == den || den == 1 = show num
    | otherwise = show num ++ "/" ++ show den
operandToString Context {fractionResult=True} (Fraction (num, den, PlusOrMinus (accNum, accDen)))
    | num == den || den == 1 = show num ++ innaccuracy
    | otherwise = show num ++ "/" ++ show den ++ innaccuracy
    where
        innaccuracy = " ± " ++ show accNum ++ "/" ++ show accDen
operandToString ctx (Fraction (num, den, accuracy))
    = decimalToString $ fractionToDecimal ctx (num, den, accuracy)

num :: Integer -> Operand
num number = Fraction (number, 1, Exact)

frac :: Integer -> Integer -> Operand
frac num den = Fraction (num, den, Exact)

var :: Char -> Operand
var letter = Variable letter
