module Operator (
    Operator,
    operatorFunction,
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

operatorToString :: Operator -> String
operatorToString operator = case operator of
    Addition -> "+"
    Subtraction -> "-"
    Multiplication -> "*"
    Division -> "/"
    Decimal -> "."
    LeftParentheses -> "("
    RightParentheses -> ")"
    Sine -> "SIN"
    Cosine -> "COS"
    Tangent -> "TAN"

stringToOperator :: String -> Operator
stringToOperator operator = case operator of
    "+" -> Addition
    "-" -> Subtraction
    "*" -> Multiplication
    "/" -> Division
    "." -> Decimal
    "(" -> LeftParentheses
    ")" -> RightParentheses
    "SIN" -> Sine
    "COS" -> Cosine
    "TAN" -> Tangent

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
    Addition -> (\y -> head y)
