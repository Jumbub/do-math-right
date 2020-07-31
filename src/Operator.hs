module Operator (
    Operator(..),
    operatorFunction,
    operatorArguments,
    operatorPrecedence,
    stringToOperator,
    operatorToString
) where

import Data.List

import Utility
import OperandType
import OperatorType
import Fraction
import Context
import Irrational

operatorToString :: Operator -> Maybe String
operatorToString operator = case operator of
    Addition -> Just "+"
    Subtraction -> Just "-"
    Multiplication -> Just "*"
    Division -> Just "/"
    Power -> Just "^"
    LeftParentheses -> Just "("
    RightParentheses -> Just ")"

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
    "PLUSORMINUS" -> Just PlusOrMinusOperator
    "PI" -> Just PiOperand
    _ -> Nothing

operatorPrecedence :: Operator -> Integer
operatorPrecedence operator = case operator of
    Addition -> 100
    Approximate -> 50
    Cosine -> 200
    Division -> 200
    LeftParentheses -> 1000
    PiOperand -> 1100
    Multiplication -> 200
    Negation -> 800
    Power -> 850
    RightParentheses -> 0
    Sine -> 200
    Subtraction -> 100
    Tangent -> 200
    PlusOrMinusOperator -> 900

operatorArguments :: Operator -> Integer
operatorArguments operator = case operator of
    Addition -> 2
    Approximate -> 1
    Cosine -> 1
    Division -> 2
    LeftParentheses -> 0
    Multiplication -> 2
    Negation -> 1
    RightParentheses -> 0
    Sine -> 1
    Subtraction -> 2
    Tangent -> 1
    PlusOrMinusOperator -> 2
    PiOperand -> 0

operatorFunction :: Operator -> ((Context, [Operand]) -> (Context, [Operand]))
operatorFunction operator = case operator of
    Addition -> contextless Operator.add
    Division -> contextless Operator.divide
    LeftParentheses -> contextless Operator.noOp
    Multiplication -> contextless Operator.multiply
    Negation -> contextless Operator.negation
    RightParentheses -> contextless Operator.noOp
    Subtraction -> contextless Operator.subtract
    Approximate -> Operator.approximate
    PlusOrMinusOperator -> contextless Operator.plusOrMinus
    Sine -> Operator.sine
    Cosine -> Operator.cosine
    Tangent -> Operator.tangent
    PiOperand -> addIrrational Pi

contextless :: ([Operand] -> [Operand]) -> ((Context, [Operand]) -> (Context, [Operand]))
contextless operation = \(ctx, operands) -> (ctx, operation operands)

operandless :: (Context -> Context) -> ((Context, [Operand]) -> (Context, [Operand]))
operandless operation = \(ctx, operands) -> (operation ctx, operands)

addIrrational :: Irrational -> ((Context, [Operand]) -> (Context, [Operand]))
addIrrational irrational = operandOperator
    where
        operandOperator :: ((Context, [Operand]) -> (Context, [Operand]))
        operandOperator (context, operands) = (context, operands ++ [Fraction $ rationalise (Context.internalDecimalPlaces context) irrational])

noOp :: [Operand] -> [Operand]
noOp input = input

add :: [Operand] -> [Operand]
add [(Fraction b), (Fraction a)] = [(Fraction $ Fraction.add a b)]

subtract :: [Operand] -> [Operand]
subtract [(Fraction b), (Fraction a)] = [(Fraction $ Fraction.subtract a b)]

multiply :: [Operand] -> [Operand]
multiply [(Fraction b), (Fraction a)] = [(Fraction $ Fraction.multiply a b)]

divide :: [Operand] -> [Operand]
divide [(Fraction b), (Fraction a)] = [(Fraction $ Fraction.divide a b)]

negation :: [Operand] -> [Operand]
negation [(Fraction fraction)] = [(Fraction $ flipSign fraction)]

approximate :: (Context, [Operand]) -> (Context, [Operand])
approximate (ctx, [x@(Fraction ((1, _), _))]) = (ctx { Context.decimalResult = True }, [x])
approximate (ctx, [x@(Fraction ((0, _), _))]) = (ctx { Context.decimalResult = False }, [x])

plusOrMinus :: [Operand] -> [Operand]
plusOrMinus [(Fraction ((bn, bd), (0, _))), (Fraction ((an, ad), (0, _)))] = [(Fraction ((an, ad), (bn, bd)))]

sine :: (Context, [Operand]) -> (Context, [Operand])
sine (ctx, [(Fraction x)]) = (ctx, [Fraction $ Fraction.sin (Context.internalDecimalPlaces ctx) x])

cosine :: (Context, [Operand]) -> (Context, [Operand])
cosine (ctx, [(Fraction x)]) = (ctx, [Fraction $ Fraction.cos (Context.internalDecimalPlaces ctx) x])

tangent :: (Context, [Operand]) -> (Context, [Operand])
tangent (ctx, [(Fraction x)]) = (ctx, [Fraction $ Fraction.tan (Context.internalDecimalPlaces ctx) x])
