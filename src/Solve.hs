module Solve (
    solve,
    performOperation,
    unsafeFraction,
) where

import Data.Char
import Data.Maybe
import Text.Read
import Data.Either
import Data.List
import Data.Sort
import Debug.Trace

import Parse
import Operator
import Operand
import Context
import Decimal
import ExactFraction (compare, ExactFraction)
import Fraction (Fraction)

-- Solve user input!

data Error =
    NotEnoughOperands |
    TooManyOperands |
    CannotAchieveAccuracy
    deriving (Show, Eq)

errorToString :: Error -> String
errorToString NotEnoughOperands = "Not enough operands!"
errorToString TooManyOperands = "Too many operands!"
errorToString CannotAchieveAccuracy = "Cannot achieve accuracy!"

unsafeError :: Either (Context, Operand) Error -> Error
unsafeError (Right x) = x

unsafeResult :: Either (Context, Operand) Error -> (Context, Operand)
unsafeResult (Left x) = x

solve :: Context -> String -> (Context, String)
solve context input
    | isRight result = (context, errorToString $ unsafeError result)
    | otherwise = toString $ unsafeResult result
    where
        firstOp = head parsed
        parsed = parse input
        result = solveUntilAccuracy Nothing context parsed

solveUntilAccuracy :: Maybe ExactFraction -> Context -> [Either Operand Operator] -> Either (Context, Operand) Error
solveUntilAccuracy lastAccuracy context parsed
    | isRight result = result
    -- Prevent infinite recursion
    | isJust lastAccuracy && ExactFraction.compare (fromJust lastAccuracy) accuracy == EQ = result
    -- If the accuracy is smaller than the required accuracy, accept the result
    | meetsAccuracy (fractionToDecimal context (unsafeFraction operand)) minimumAccuracy = result
    -- Recurse until result satisfies accuracy requirement
    | otherwise = solveUntilAccuracy (Just accuracy) moreAccurateContext parsed
    where
        result = simplify context parsed [] []
        (_, operand) = unsafeResult result
        (_, accuracy) = unsafeFraction operand
        currentDecimals = internalDecimalPlaces context
        minimumAccuracy = (1, 10 ^ (decimalPlaces context))
        moreAccurateContext = context {internalDecimalPlaces = (currentDecimals + 1)}

meetsAccuracy :: Decimal -> ExactFraction -> Bool
meetsAccuracy (RecurringDecimal _) _ = True
meetsAccuracy (ExactDecimal _) _ = True
meetsAccuracy (PlusOrMinusDecimal (_, _, _, xp)) minAcc = ExactFraction.compare xp minAcc /= GT

simplify :: Context -> [Either Operand Operator] -> [Operand] -> [Operator] -> Either (Context, Operand) Error
simplify ctx terms operands operators
    | null terms && null operators && null operands = Right NotEnoughOperands
    | null terms && null operators && length operands > 1 = Right TooManyOperands
    | null terms && null operators = Left $ (ctx, head operands)
    | null terms = doOperationFromStack
    | isRightParentheses && nextOnStackLeftParentheses = simplify ctx (tail terms) operands (tail operators)
    | isRightParentheses = doOperationFromStack
    | isOperand = simplify ctx (tail terms) (operand : operands) operators
    | useOperatorsInStack = doOperationFromStack
    | otherwise = simplify ctx (tail terms) operands (operator : operators)
    where
        doOperationFromStack = simplify ctx' terms operands' (tail operators)
        (ctx', operands') = performOperation ctx (head operators) operands
        useOperatorsInStack = if (null operators || ((head operators) == LeftParentheses))
            then False
            else (operatorPrecedence operator) <= (operatorPrecedence (head operators))
        term = head terms
        isRightParentheses = isOperator && operator == RightParentheses
        nextOnStackLeftParentheses = (head operators) == LeftParentheses
        isOperand = isLeft term
        operand = fromLeft (num 0) term
        isOperator = isRight term
        operator = fromRight Addition term
        lastOperator = fromRight Addition (head terms)

toString :: (Context, Operand) -> (Context, String)
toString (context, operand) = (context, operandToString context operand)

-- Perform an operation on the operand stack

performOperation ::  Context -> Operator -> [Operand] -> (Context, [Operand])
performOperation ctx operation operands
    | notEnoughOperands = error "Not enough operands to perform operation!"
    | otherwise = (ctx', arguments' ++ genericDrop numArguments operands)
    where
        (ctx', arguments') = function (ctx, arguments)
        numArguments = operatorArguments operation
        arguments = genericTake numArguments operands
        notEnoughOperands = genericLength arguments /= numArguments
        function = operatorFunction operation
