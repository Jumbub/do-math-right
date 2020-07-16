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

-- Solve user input!

solve :: Context -> String -> (Context, String)
solve ctx input = (ctx', operandToString ctx' output)
    where
        (ctx', output) = solve' ctx (parse input) [] []

solve' :: Context -> [Either Operand Operator] -> [Operand] -> [Operator] -> (Context, Operand)
solve' ctx terms operands operators
    | null terms && null operators && null operands = error "No resulting operands!"
    | null terms && null operators && length operands > 1 = error "Too many resulting operands!"
    | null terms && null operators = (ctx, head operands)
    | null terms = doOperationFromStack
    | isRightParentheses && nextOnStackLeftParentheses = solve' ctx (tail terms) operands (tail operators)
    | isRightParentheses = doOperationFromStack
    | isOperand = solve' ctx (tail terms) (operand : operands) operators
    | useOperatorsInStack = doOperationFromStack
    | otherwise = solve' ctx (tail terms) operands (operator : operators)
    where
        doOperationFromStack = solve' ctx' terms operands' (tail operators)
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
