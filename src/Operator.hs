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

data Operator =

    Addition |
    Subtraction |
    Multiplication |
    Division |

    LeftParentheses |
    RightParentheses |

    Decimal |
    Negation |

    Sine |
    Cosine |
    Tangent

    deriving (Eq, Show)

operatorToString :: Operator -> Maybe String
operatorToString operator = case operator of
    Addition -> Just "+"
    Subtraction -> Just "-"
    Multiplication -> Just "*"
    Division -> Just "/"
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
add [((bNum, bDen), bVars), ((aNum, aDen), aVars)] = [((numerator, denominator), aVars)]
    where
        numerator = aNum * bDen + bNum * aDen
        denominator = aDen * bDen

subtract :: [Operand] -> [Operand]
subtract [((bNum, bDen), bVars), ((aNum, aDen), aVars)] = [((numerator, denominator), aVars)]
    where
        numerator = aNum * bDen - bNum * aDen
        denominator = aDen * bDen

multiply :: [Operand] -> [Operand]
multiply [((bNum, bDen), (bNumVars, bDenVars)), ((aNum, aDen), (aNumVars, aDenVars))] = [((numerator, denominator), (numeratorVars, denominatorVars))]
    where
        numerator = aNum * bNum
        denominator = aDen * bDen
        numeratorVars = uniqueValues (aNumVars ++ bNumVars)
        denominatorVars = uniqueValues (aDenVars ++ bDenVars)

divide :: [Operand] -> [Operand]
divide [((bNum, bDen), bVars), ((aNum, aDen), aVars)] = [((numerator, denominator), aVars)]
    where
        numerator = aNum * bDen
        denominator = aDen * bNum

decimal :: [Operand] -> [Operand]
decimal [((b, 1), ([], [])), ((a, 1), ([], []))] = [((numerator, denominator), ([], []))]
    where
        decimalPlaces = length (show b)
        numerator = (a * decimalPlaces) + b
        denominator = 10 ^ decimalPlaces

negation :: [Operand] -> [Operand]
negation [((aNum, aDen), aVars)] = [((-aNum, aDen), aVars)]
