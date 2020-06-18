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

solve' :: [Either Operand Operator] -> [Operand] -> [Operator] -> Operand
solve' terms operands operators
    | null terms && null operators && null operands = error "No resulting operands!"
    | null terms && null operators && length operands > 1 = error "Too many resulting operands!"
    | null terms && null operators = head operands
    | null terms = solve' [] (performOperation (head operators) operands) (tail operators)
    | isOperand = solve' (tail terms) (operand : operands) operators
    | doLastOperation = solve' terms (performOperation lastOperator operands) operators
    | otherwise = solve' (tail terms) operands (operator : operators)
    where
        doLastOperation = if (null operators)
            then False
            else (operatorPrecedence operator) < (operatorPrecedence (head operators))
        term = head terms
        isOperand = isLeft term
        operand = fromLeft (num 0) term
        isOperator = isRight term
        operator = fromRight Addition term
        lastOperator = fromRight Addition (head terms)

-- Perform an operation on the operand stack

performOperation ::  Operator -> [Operand] -> [Operand]
performOperation operation operands
    | notEnoughOperands = error "Not enough operands to perform operation!"
    | otherwise = (drop numArguments operands) ++ [function arguments]
    where
        numArguments = operatorArguments operation
        arguments = take numArguments operands
        notEnoughOperands = length arguments /= numArguments
        function = operatorFunction operation
