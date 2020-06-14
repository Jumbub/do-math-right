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
      concat $ map (\x -> seperateWhenDifferent notBothUpperOrDigits x)
    $ concat $ map (\x -> seperateWhenDifferent notBothUpper x)
    $ concat $ map (\x -> seperateWhenDifferent notBothDigits x) [singleString]
     where
        notBothDigits = \a b -> xor (isDigit a) (isDigit b)
        notBothUpper = \a b -> xor (isUpper a) (isUpper b)
        notBothUpperOrDigits = \a b -> not (isDigit a && isDigit b || isUpper a && isUpper b)

-- "aBCdeF" => ["a", "BC", "d", "F"], where identifier is `isUpper`
seperateWhenDifferent :: (Char -> Char -> Bool) -> String -> [String]
seperateWhenDifferent isDifferent input = seperateWhenDifferent' isDifferent input Nothing "" []

seperateWhenDifferent' :: (Char -> Char -> Bool) -> String -> Maybe Char -> String -> [String] -> [String]
seperateWhenDifferent' _ [] _ [] splits = splits
seperateWhenDifferent' _ [] _ group splits = splits ++ [group]
seperateWhenDifferent' isDifferent remaining Nothing group splits = seperateWhenDifferent' isDifferent (tail remaining) (Just current) [current] []
    where
        current = head remaining
seperateWhenDifferent' isDifferent remaining (Just last) group splits
    | not $ isDifferent last current = seperateWhenDifferent' isDifferent nextRemaining (Just current) (group ++ [current]) splits
    | otherwise = seperateWhenDifferent' isDifferent nextRemaining (Just current) [current] (splits ++ [group])
    where
        current = head remaining
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
