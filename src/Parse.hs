module Parse (
    parse,
    cleanInput,
    splitInput,
    parseSplits,
    addImplicitOperations,
) where

import Data.Char
import Data.Bits
import Data.Maybe
import Text.Read
import Data.Either

import Operand
import Operator

parse :: String -> ([Operand], [Operator])
parse input = ([], [])



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

-- E.g. "aBCdeF" => ["a", "BC", "d", "F"], where identifier is `isUpper`
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

parseSplits :: [String] -> [Either Operand Operator]
parseSplits input = map parseSplit input

parseSplit :: String -> Either Operand Operator
parseSplit split
    | isJust operand = Left (fromJust operand)
    | isJust operator = Right (fromJust operator)
    | otherwise = error ("cannot parse split '" ++ split ++ "'")
    where
        operand = stringToOperand split
        operator = stringToOperator split

-- Split input #2
-- ["5x", "SIN", "-", "1", "NEG"] => [([5, "x"], [MULTIPLY]), SINE, SUBTRACT, 1, NEGATE]
parseOperands :: [Either Operand String] -> [Either Operand Operator]
parseOperands input = []



-- Simplify input!

cleanInput :: String -> String
cleanInput expression = functionsToPostfix $ convertToFunctions expression

-- Simplify input #1
-- "SIN(5x)-(-1)" => "SIN(5x)-NEG(1)"
convertToFunctions :: String -> String
convertToFunctions expression = expression

-- Simplify input #2
-- "SIN(5x)-NEG(1)" => "5xSIN-1NEG"
functionsToPostfix :: String -> String
functionsToPostfix expression = expression



-- Add implicit operations!

addImplicitOperations :: [Either Operand Operator] -> [Either Operand Operator]
addImplicitOperations input = addImplicitMultiply input

-- Add implicit operations #2
-- [5, "x"] => [5, Multiplication, "x"]
addImplicitMultiply :: [Either Operand Operator] -> [Either Operand Operator]
addImplicitMultiply input = addImplicitMultiply' False input

addImplicitMultiply' :: Bool -> [Either Operand Operator] -> [Either Operand Operator]
addImplicitMultiply' lastOperand expression
    | null expression = []
    | lastOperand = [Right Multiplication, head expression] ++ addImplicitMultiply' thisOperand (tail expression)
    | otherwise = [head expression] ++ addImplicitMultiply' thisOperand (tail expression)
    where
        thisOperand = isLeft $ head expression
