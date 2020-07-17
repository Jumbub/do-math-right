module OperandType (
    Operand(..),
    Expression(..),
) where

import Fraction
import OperatorType
import IrrationalType

type Variable = Char

data Expression =
    Fraction Fraction |
    Variable Variable |
    Irrational Irrational |
    Expression ([Expression], Operator)
    deriving (Eq, Show)

type Operand = Expression
