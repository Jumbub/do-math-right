module Parse
    (
        parse,
        simplifyInput,
        splitInput,
        parseSplitInput,
        addImplicitOperations,
    ) where

import Definitions
import Data.Char
import Data.Maybe
import Text.Read

type OperandOrOperator = [Either Operand Operator]

parse :: String -> ([Operand], [Operator])
parse input = ([], [])



-- Simplify input!

simplifyInput :: String -> String
simplifyInput expression = functionsToPostfix $ convertToFunctions expression

-- Simplify input #1
-- "\sin(5x)-(-1)" => "\sin(5x)-\neg(1)"
convertToFunctions :: String -> String
convertToFunctions expression = expression

-- Simplify input #2
-- "\sin(5x)-\neg(1)" => "5x`sin`-1`neg`"
functionsToPostfix :: String -> String
functionsToPostfix expression = expression



-- Split input!

splitInput :: String -> [String]
splitInput input = splitOperators $ splitFunctions $ splitNumbers input

-- Split input #1
-- "5x`sin`-1`neg`" => ["5", "`sin`-", "1", "`neg`"]
splitNumbers:: String -> [String]
splitNumbers input = [input]

-- Split input #2
-- ["5", "`sin`-", "1", "`neg`"] => ["5", "`sin`", "-", "1", "`neg`"]
splitFunctions :: [String] -> [String]
splitFunctions input = input

-- Split input #3
-- [")-"] => [")", "-"]
splitOperators :: [String] -> [String]
splitOperators input = input



-- Parse split input!

parseSplitInput :: [String] -> [OperandOrOperator]
parseSplitInput input = parseOperands $ parseOperators input

-- Split input #2
-- ["5x", "`sin`", "-", "1", "`neg`"] => ["5x", SINE, SUBTRACT, "1", NEGATE]
parseOperators :: [String] -> [Either Operand String]
parseOperators input = []

-- Split input #2
-- ["5x", "`sin`", "-", "1", "`neg`"] => [([5, "x"], [MULTIPLY]), SINE, SUBTRACT, 1, NEGATE]
parseOperands :: [Either Operand String] -> [OperandOrOperator]
parseOperands input = []



-- Add implicit operations!

addImplicitOperations :: [OperandOrOperator] -> [OperandOrOperator]
addImplicitOperations input = addImplicitMultiply input

-- Add implicit operations #2
-- [5, "x"] => [5, MULTIPLY, "x"]
addImplicitMultiply :: [OperandOrOperator] -> [OperandOrOperator]
addImplicitMultiply input = input
