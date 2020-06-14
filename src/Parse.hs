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
import Data.Bits
import Data.Maybe
import Text.Read

type OperandOrOperator = [Either Operand Operator]

parse :: String -> ([Operand], [Operator])
parse input = ([], [])



-- Simplify input!

simplifyInput :: String -> String
simplifyInput expression = functionsToPostfix $ convertToFunctions expression

-- Simplify input #1
-- "SIN(5x)-(-1)" => "SIN(5x)-NEG(1)"
convertToFunctions :: String -> String
convertToFunctions expression = expression

-- Simplify input #2
-- "SIN(5x)-NEG(1)" => "5xSIN-1NEG"
functionsToPostfix :: String -> String
functionsToPostfix expression = expression



-- Split input!

splitInput :: String -> [String]
splitInput input = splitOperators $ splitFunctions $ splitNumbers input

-- Split input #1
-- "5xSIN-1NEG" => ["5", "SIN-", "1", "NEG"]
splitNumbers :: String -> [String]
splitNumbers input = splitNumbers' input Nothing "" []

splitNumbers' :: String -> Maybe Bool -> String -> [String] -> [String]
splitNumbers' [] _ [] splits = splits
splitNumbers' [] _ group splits = splits ++ [group]
splitNumbers' remaining Nothing group splits = splitNumbers' (tail remaining) (Just $ isDigit $ current) [current] []
    where
        current = head remaining
splitNumbers' remaining (Just lastDigit) group splits
    | lastDigit == currentDigit = splitNumbers' nextRemaining (Just currentDigit) (group ++ [current]) splits
    | otherwise = splitNumbers' nextRemaining (Just currentDigit) [current] (splits ++ [group])
    where
        current = head remaining
        currentDigit = isDigit current
        nextRemaining = tail remaining

-- Split input #2
-- ["5", "SIN-", "1", "NEG"] => ["5", "SIN", "-", "1", "NEG"]
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
-- ["5x", "SIN", "-", "1", "NEG"] => ["5x", SINE, SUBTRACT, "1", NEGATE]
parseOperators :: [String] -> [Either Operand String]
parseOperators input = []

-- Split input #2
-- ["5x", "SIN", "-", "1", "NEG"] => [([5, "x"], [MULTIPLY]), SINE, SUBTRACT, 1, NEGATE]
parseOperands :: [Either Operand String] -> [OperandOrOperator]
parseOperands input = []



-- Add implicit operations!

addImplicitOperations :: [OperandOrOperator] -> [OperandOrOperator]
addImplicitOperations input = addImplicitMultiply input

-- Add implicit operations #2
-- [5, "x"] => [5, MULTIPLY, "x"]
addImplicitMultiply :: [OperandOrOperator] -> [OperandOrOperator]
addImplicitMultiply input = input
