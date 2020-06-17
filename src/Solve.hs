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
performOperation operation operands = result
    where
        result = []
