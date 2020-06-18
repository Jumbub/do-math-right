module Solve (
    solve,
    performOperation,
) where

import Data.Char
import Data.Maybe
import Text.Read
import Data.Either

import Parse
import Operator
import Operand

-- Solve user input!

solve :: String -> String
solve input = solvedAsString
    where
        solvedAsString = operandToString solved
        solved = solve' parsed [] []
        parsed = parse input

solve' :: [Either Operand Operator] -> [Operand] -> [[Operator]] -> Operand
solve' terms operandStack operatorStacks
    | null terms = if length operandStack == 1 then head operandStack else error "Oh no!"
    | isOperand = solve' (tail terms) (operandStack ++ [operand]) operatorStacks
    | otherwise = error "Failed to solve expression!"
    where
        term = head terms
        isOperand = isLeft term
        operand = fromLeft (num 0) term
        isOperator = isRight term
        operator = fromRight Multiplication term

-- Perform an operation on the operand stack

performOperation ::  Operator -> [Operand] -> [Operand]
performOperation operation operands
    | notEnoughOperands = error "Attempting to perform operation without enough arguments"
    | otherwise = (drop numArguments operands) ++ [function arguments]
    where
        numArguments = operatorArguments operation
        arguments = take numArguments operands
        notEnoughOperands = length arguments /= numArguments
        function = operatorFunction operation
