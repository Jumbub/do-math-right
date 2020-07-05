module Parse (
    parse,
    cleanInput,
    groupSplits,
    splitInput,
    parseSplits,
    addImplicitOperations,
) where

import Data.Char
import Data.Bits
import Data.Maybe
import Text.Read
import Data.Either
import Debug.Trace

import Operand
import Operator

parse :: String -> [Either Operand Operator]
parse input = addImplicitOperations $ parseSplits $ groupSplits $ splitInput $ cleanInput input

-- Clean input!

cleanInput :: String -> String
cleanInput input = filter (\y -> y /= ' ') input

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

seperateWhenDifferent :: (Char -> Char -> Bool) -> String -> [String]
seperateWhenDifferent isDifferent input = seperateWhenDifferent' isDifferent input Nothing "" []

seperateWhenDifferent' :: (Char -> Char -> Bool) -> String -> Maybe Char -> String -> [String] -> [String]
seperateWhenDifferent' _ [] _ [] splits = splits
seperateWhenDifferent' _ [] _ group splits = splits ++ [group]
seperateWhenDifferent' isDifferent remaining Nothing group splits = diffResult
    where
        current = head remaining
        diffResult = seperateWhenDifferent' isDifferent (tail remaining) (Just current) [current] []
seperateWhenDifferent' isDifferent remaining (Just last) group splits
    | not $ isDifferent last current = seperateWhenDifferent' isDifferent nextRemaining (Just current) (group ++ [current]) splits
    | otherwise = seperateWhenDifferent' isDifferent nextRemaining (Just current) [current] (splits ++ [group])
    where
        current = head remaining
        nextRemaining = tail remaining

-- Group splts!

groupSplits :: [String] -> [String]
groupSplits (a:".":b:rest) = [a ++ "." ++ b] ++ groupSplits rest
groupSplits (x:rest) = (x:groupSplits rest)
groupSplits [] = []

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

-- Add implicit operations!

addImplicitOperations :: [Either Operand Operator] -> [Either Operand Operator]
addImplicitOperations input = replaceSubtractionWithNegation $ addImplicitMultiplication input

-- This value is just used as a default for the parentheses
notParentheses = Right Sine

-- Add implicit operations #2
-- (2)x => (2)*x
addImplicitMultiplication :: [Either Operand Operator] -> [Either Operand Operator]
addImplicitMultiplication input = addImplicitMultiplication' notParentheses input

addImplicitMultiplication' :: Either Operand Operator -> [Either Operand Operator] -> [Either Operand Operator]
addImplicitMultiplication' lastTerm expression
    | null expression = []
    | insert = [Right Multiplication, currentTerm] ++ addImplicitMultiplication' currentTerm (tail expression)
    | otherwise = [currentTerm] ++ addImplicitMultiplication' currentTerm (tail expression)
    where
        currentTerm = head expression
        insert =
            lastTerm == Right RightParentheses && isLeft currentTerm
            || isLeft lastTerm && currentTerm == Right LeftParentheses
            || isLeft lastTerm && isLeft currentTerm

-- Add implicit operations #2
-- (-1) => (Negation 1)
replaceSubtractionWithNegation :: [Either Operand Operator] -> [Either Operand Operator]
replaceSubtractionWithNegation input = tail $ replaceSubtractionWithNegation' (Right LeftParentheses:input)

replaceSubtractionWithNegation' :: [Either Operand Operator] -> [Either Operand Operator]
replaceSubtractionWithNegation' [] = []
replaceSubtractionWithNegation' (Right LeftParentheses:Right Subtraction:rest) = [Right LeftParentheses, Right Negation] ++ (replaceSubtractionWithNegation' rest)
replaceSubtractionWithNegation' (current:rest) = [current] ++ (replaceSubtractionWithNegation' rest)
