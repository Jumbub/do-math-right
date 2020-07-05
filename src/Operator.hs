module Operator (
    Operator(..),
    operatorFunction,
    operatorArguments,
    operatorPrecedence,
    stringToOperator,
    operatorToString
) where

import Data.List

import Operand
import Utility
import Type

operatorToString :: Operator -> Maybe String
operatorToString operator = case operator of
    Addition -> Just "+"
    Subtraction -> Just "-"
    Multiplication -> Just "*"
    Division -> Just "/"
    Power -> Just "^"
    LeftParentheses -> Just "("
    RightParentheses -> Just ")"
    Sine -> Just "SIN"
    Cosine -> Just "COS"
    Tangent -> Just "TAN"

stringToOperator :: String -> Maybe Operator
stringToOperator operator = case operator of
    "+" -> Just Addition
    "-" -> Just Subtraction
    "*" -> Just Multiplication
    "/" -> Just Division
    "^" -> Just Power
    "(" -> Just LeftParentheses
    ")" -> Just RightParentheses
    "SIN" -> Just Sine
    "COS" -> Just Cosine
    "TAN" -> Just Tangent
    "APPROXIMATE" -> Just Approximate
    _ -> Nothing

operatorPrecedence :: Operator -> Integer
operatorPrecedence operator = case operator of
    Addition -> 100
    Approximate -> 50
    Cosine -> 300
    Division -> 200
    LeftParentheses -> 1000
    Multiplication -> 200
    Negation -> 800
    Power -> 850
    RightParentheses -> 0
    Sine -> 300
    Subtraction -> 100
    Tangent -> 300

operatorArguments :: Operator -> Integer
operatorArguments operator = case operator of
    Addition -> 2
    Approximate -> 0
    Cosine -> 1
    Division -> 2
    LeftParentheses -> 0
    Multiplication -> 2
    Negation -> 1
    RightParentheses -> 0
    Sine -> 1
    Subtraction -> 2
    Tangent -> 1

operatorFunction :: Operator -> ((Context, [Operand]) -> (Context, [Operand]))
operatorFunction operator = case operator of
    Addition -> simplifyable $ contextless add
    Division -> simplifyable $ contextless divide
    LeftParentheses -> contextless noOp
    Multiplication -> contextless multiply
    Negation -> simplifyable $ contextless negation
    RightParentheses -> contextless noOp
    Subtraction -> simplifyable $ contextless Operator.subtract

contextless :: ([Operand] -> [Operand]) -> ((Context, [Operand]) -> (Context, [Operand]))
contextless operation = \(ctx, operands) -> (ctx, operation operands)

operandless :: (Context -> Context) -> ((Context, [Operand]) -> (Context, [Operand]))
operandless operation = \(ctx, operands) -> (operation ctx, operands)

simplifyable :: ((Context, [Operand]) -> (Context, [Operand])) -> ((Context, [Operand]) -> (Context, [Operand]))
simplifyable operation = \(ctx, operands) -> (Operator.simplifyFraction $ operation (ctx, operands))

simplifyFraction :: (Context, [Operand]) -> (Context, [Operand])
simplifyFraction (ctx, [Fraction (num, den, acc)]) = (ctx, [Fraction (numerator, denominator, acc)])
    where
        (numerator, denominator) = Utility.simplifyFraction (num, den)

noOp :: [Operand] -> [Operand]
noOp input = input

add :: [Operand] -> [Operand]
add [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen + bNum * aDen
        denominator = aDen * bDen

subtract :: [Operand] -> [Operand]
subtract [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen - bNum * aDen
        denominator = aDen * bDen

multiply :: [Operand] -> [Operand]
multiply [(Fraction (bNum, bDen, bAcc)), (Fraction (aNum, aDen, aAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bNum
        denominator = aDen * bDen
multiply [(Variable b), (Variable a)]
    | a == b = [Expression ([Variable a, num 2], Power)]
    | otherwise = [Expression ([Variable a, Variable b], Multiplication)]

divide :: [Operand] -> [Operand]
divide [(Fraction (bNum, bDen, aAcc)), (Fraction (aNum, aDen, bAcc))] = [(Fraction (numerator, denominator, aAcc))]
    where
        numerator = aNum * bDen
        denominator = aDen * bNum

negation :: [Operand] -> [Operand]
negation [(Fraction (num, den, accuracy))] = [(Fraction (-num, den, accuracy))]

approximate :: Context -> Context
approximate ctx = ctx { Type.fractionResult = False }
