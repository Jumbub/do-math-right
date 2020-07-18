module OperandType (
    Operand(..),
    Expression(..),
) where

import Fraction
import OperatorType
import IrrationalType

data Expression =
    Fraction Fraction
    deriving (Eq, Show)

type Operand = Expression
