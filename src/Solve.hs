module Solve (
    solve,
    performOperation,
) where

import Data.Char
import Data.Maybe
import Text.Read
import Data.Either
import Data.List

import Parse
import Operator
import Operand
import Context
import Fraction (Fraction)

-- Solve user input!

data Error =
    NotEnoughOperands |
    TooManyOperands
    deriving (Show, Eq)

errorToString :: Error -> String
errorToString NotEnoughOperands = "Not enough operands!"
errorToString TooManyOperands = "Too many operands!"



solve :: Context -> String -> (Context, String)
solve context input
    | isRight simplified = (context, errorToString $ unsafeError simplified)
    | fractionResult context = toString $ unsafeResult simplified
    | otherwise = toString $ Solve.approximate $ unsafeResult simplified
    where
        firstOp = head parsed
        parsed = parse input
        simplified = simplify context parsed [] []
        unsafeError :: Either (Context, Operand) Error -> Error
        unsafeError (Right x) = x
        unsafeResult :: Either (Context, Operand) Error -> (Context, Operand)
        unsafeResult (Left x) = x

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

approximate :: (Context, Operand) -> (Context, Operand)
approximate (context, operand) = (context, operand)

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
