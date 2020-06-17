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
solve input = "success"

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
