module Operators (operatorFunction) where

import Definitions

operatorFunction :: Operator -> (Operand -> Operand -> Operand)
operatorFunction operator = case operator of
    Addition -> (+)
    Subtraction -> (-)
    Multiplication -> (*)
    Division -> div
