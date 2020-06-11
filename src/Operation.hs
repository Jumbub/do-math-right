module Operation (
    Operator(..),
    operationPrecedence,
    Operand,
    Fact,
    Result,
) where

-- TODO: this will be changed to a burrito at some point
-- because it will represent values such as `x + y`
type Operand = Integer

type Fact = (String, Operand)

type Result = Either Fact Operand

data Operator =
    Addition |
    Subtraction |
    Multiplication |
    Division
    deriving (Eq)

operationPrecedence :: Operator -> Int
operationPrecedence operation = case operation of
    Addition -> 100
    Subtraction -> 100
    Multiplication -> 200
    Division -> 200

operationFunction :: Operator -> (Operand -> Operand -> Operand)
operationFunction operation = case operation of
    Addition -> (+)
    Subtraction -> (-)
    Multiplication -> (*)
    Division -> div
