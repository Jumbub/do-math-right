module Operand (
    Operand,
    stringToOperand,
) where

type Operand = ((Int, Int), ([Char], [Char]))

stringToOperand :: String -> Maybe Operand
stringToOperand input = Just ((0, 1), ([], []))
