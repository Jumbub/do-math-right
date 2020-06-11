module Solve
    (
        solve
    ) where

import Data.Char
import Data.Maybe
import Text.Read

import Definitions
import Parse

-- Solve user input!

solve :: String -> [Fact] -> (String, Maybe Fact)
solve stringInput facts = (resultToString solution, factOrNothing solution)
    where
        parsedInput = parse stringInput
        operandsWithFacts = fillOperandsWithFacts facts $ fst parsedInput
        parsedWithFacts = (operandsWithFacts, snd parsedInput)
        solution = solveParsed parsedWithFacts

factOrNothing :: Result -> Maybe Fact
factOrNothing result
    -- TODO: Figure out why when I change the below to return "Just result" it still thinks
    -- that the value of result could be an operand.
    | isFact result = Just ("x", 0)
    | otherwise = Nothing

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
