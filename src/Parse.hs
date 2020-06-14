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
splitInput singleString =
      concat $ map (\x -> seperateString isNotDigitOrUpper x)
    $ concat $ map (\x -> seperateString isDigit x)
    $ concat $ map (\x -> seperateString isUpper x) [singleString]
    where
        isNotDigitOrUpper = (\y -> (not (isDigit y) && not (isUpper y)))

-- "aBCdeF" => ["a", "BC", "d", "F"], where identifier is `isUpper`
seperateString :: (Char -> Bool) -> String -> [String]
seperateString getType input = seperateString' getType input Nothing "" []

seperateString' :: (Char -> Bool) -> String -> Maybe Bool -> String -> [String] -> [String]
seperateString' _ [] _ [] splits = splits
seperateString' _ [] _ group splits = splits ++ [group]
seperateString' getType remaining Nothing group splits = seperateString' getType (tail remaining) (Just $ getType $ current) [current] []
    where
        current = head remaining
seperateString' getType remaining (Just lastValue) group splits
    | lastValue == currentValue = seperateString' getType nextRemaining (Just currentValue) (group ++ [current]) splits
    | otherwise = seperateString' getType nextRemaining (Just currentValue) [current] (splits ++ [group])
    where
        current = head remaining
        currentValue = getType current
        nextRemaining = tail remaining



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
