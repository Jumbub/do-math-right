module Definitions (
    Operator(..),
    operatorPrecedence,
    Operand,
    Fact,
    Result,
    isFact,
    isOperand,
) where

import Data.Either

type Operand = Integer

type Fact = (String, Operand)

type Result = Either Fact Operand

isFact = isLeft
isOperand = isRight

data Operator =
    Addition |
    Subtraction |
    Multiplication |
    Division
    deriving (Eq)

operatorPrecedence :: Operator -> Int
operatorPrecedence operator = case operator of
    Addition -> 100
    Subtraction -> 100
    Multiplication -> 200
    Division -> 200
