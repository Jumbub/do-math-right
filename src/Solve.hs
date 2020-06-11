module Solve
    (
        solve
    ) where

import Operation
import Data.Char
import Data.Maybe
import Text.Read

import Operation
import Parse

-- Solve user input!

solve :: String -> [Fact] -> String
solve stringInput facts = resultToString $ solveParsed parsedWithFacts
    where
        parsedInput = parse stringInput
        operandsWithFacts = fillOperandsWithFacts facts $ fst parsedInput
        parsedWithFacts = (operandsWithFacts, snd parsedInput)

resultToString :: Result -> String
resultToString result = ""

-- Solve a parsed equation!
-- (["x", "1"], ["+"]), [("x", 5)] => 6
-- (["x"], ["1"]), [("=")] => ("x", "1")
solveParsed :: ([Operand], [Operator]) -> Result
solveParsed expression = Right 0

-- [("x", 5)], ["x"] => ["5"]
fillOperandsWithFacts :: [Fact] -> [Operand] -> [Operand]
fillOperandsWithFacts facts operands = []
