module OperandType (
    Operand(..),
    Expression(..),
) where

import Fraction
import OperatorType

type Variable = Char

data Expression =
    Fraction Fraction |
    Variable Variable |
    Expression ([Expression], Operator)
    deriving (Eq, Show)

type Operand = Expression
