module Operator (
    Operator(..),
    operatorFunction,
    operatorArguments,
    operatorPrecedence,
    stringToOperator,
    operatorToString
) where

import Operand
import Utility
import Type

operatorToString :: Operator -> Maybe String
operatorToString operator = case operator of
    Addition -> Just "+"
    Subtraction -> Just "-"
    Multiplication -> Just "*"
    Division -> Just "/"
    Power -> Just "^"
    Decimal -> Just "."
    LeftParentheses -> Just "("
    RightParentheses -> Just ")"
    Sine -> Just "SIN"
    Cosine -> Just "COS"
    Tangent -> Just "TAN"

stringToOperator :: String -> Maybe Operator
stringToOperator operator = case operator of
    "+" -> Just Addition
    "-" -> Just Subtraction
    "*" -> Just Multiplication
    "/" -> Just Division
    "." -> Just Decimal
    "^" -> Just Power
    "(" -> Just LeftParentheses
    ")" -> Just RightParentheses
    "SIN" -> Just Sine
    "COS" -> Just Cosine
    "TAN" -> Just Tangent
    _ -> Nothing

operatorPrecedence :: Operator -> Int
operatorPrecedence operator = case operator of
    Addition -> 100
    Subtraction -> 100
    Multiplication -> 200
    Division -> 200
    Power -> 850
    Decimal -> 900
    Negation -> 800
    LeftParentheses -> 1000
    RightParentheses -> 0
    Sine -> 300
    Cosine -> 300
    Tangent -> 300

operatorArguments :: Operator -> Int
operatorArguments operator = case operator of
    Addition -> 2
    Subtraction -> 2
    Multiplication -> 2
    Division -> 2
    Decimal -> 2
    Negation -> 1
    LeftParentheses -> 0
    RightParentheses -> 0
    Sine -> 1
    Cosine -> 1
    Tangent -> 1

operatorFunction :: Operator -> ([Operand] -> [Operand])
operatorFunction operator = case operator of
    Addition -> add
    Subtraction -> Operator.subtract
    Multiplication -> multiply
    Division -> divide
    LeftParentheses -> noOp
    RightParentheses -> noOp
    Decimal -> decimal
    Negation -> negation

noOp :: [Operand] -> [Operand]
noOp input = input

add :: [Operand] -> [Operand]
add [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen + bNum * aDen
        denominator = aDen * bDen

subtract :: [Operand] -> [Operand]
subtract [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen - bNum * aDen
        denominator = aDen * bDen

multiply :: [Operand] -> [Operand]
multiply [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bNum
        denominator = aDen * bDen
multiply [(Variable b), (Variable a)]
    | a == b = [Expression ([Variable a, num 2], Power)]
    | otherwise = [Expression ([Variable a, Variable b], Multiplication)]

divide :: [Operand] -> [Operand]
divide [(Fraction (bNum, bDen, aAcc)), (Fraction (aNum, aDen, bAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen
        denominator = aDen * bNum

decimal :: [Operand] -> [Operand]
decimal [(Fraction (b, 1, bAcc)), (Fraction (a, 1, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        decimalPlaces = length (show b)
        numerator = (a * decimalPlaces) + b
        denominator = 10 ^ decimalPlaces

negation :: [Operand] -> [Operand]
negation [(Fraction (num, den, accuracy))] = [(Fraction (-num, den, accuracy))]
