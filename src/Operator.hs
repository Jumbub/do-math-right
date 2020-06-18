module Operator (
    Operator(..),
    operatorFunction,
    operatorArguments,
    operatorPrecedence,
    stringToOperator,
    operatorToString
) where

import Operand

data Operator =

    Addition |
    Subtraction |
    Multiplication |
    Division |

    LeftParentheses |
    RightParentheses |

    Decimal |

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
    _ -> Nothing

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
    LeftParentheses -> 50
    RightParentheses -> 1000
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
    LeftParentheses -> 0
    RightParentheses -> 0
    Sine -> 1
    Cosine -> 1
    Tangent -> 1

operatorFunction :: Operator -> ([Operand] -> Operand)
operatorFunction operator = case operator of
    Addition -> add
    Subtraction -> Operator.subtract
    Multiplication -> multiply

add :: [Operand] -> Operand
add [((aNum, aDen), aVars), ((bNum, bDen), bVars)] = ((numerator, denominator), aVars)
    where
        numerator = aNum * bDen + bNum * aDen
        denominator = aDen * bDen

subtract :: [Operand] -> Operand
subtract [((aNum, aDen), aVars), ((bNum, bDen), bVars)] = ((numerator, denominator), aVars)
    where
        numerator = aNum * bDen - bNum * aDen
        denominator = aDen * bDen

multiply :: [Operand] -> Operand
multiply [((aNum, aDen), aVars), ((bNum, bDen), bVars)] = ((numerator, denominator), aVars)
    where
        numerator = aNum * bNum
        denominator = aDen * bDen

