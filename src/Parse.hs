module Parse
    (
        parse,
        simplifyInput,
        splitInput,
        parseOperatorInput,
        parseOperandInput
    ) where

import Definitions
import Data.Char
import Data.Maybe
import Text.Read

parse :: String -> ([Operand], [Operator])
parse input = ([], [])


-- Simplify input!

simplifyInput :: String -> String
simplifyInput expression = functionsToPostfix $ convertToFunctions expression

-- Simplify input #1
-- "\sin(5x)-(-1)" => "\sin(5x)-\neg(1)"
convertToFunctions :: String -> String
convertToFunctions expression = ""

-- Simplify input #2
-- "\sin(5x)-\neg(1)" => "5x`sin`-1`neg`"
functionsToPostfix :: String -> String
functionsToPostfix expression = ""

-- "\sin(x)" => "(x)sin\"
moveFunctionToAfterBracket :: String -> String -> String
moveFunctionToAfterBracket expression replacement = ""

-- "a(b(c)d)e" => "ab(c)d)e"
removeNextOpeningBracket :: String -> String -> String
removeNextOpeningBracket expression replacement = ""

-- "a(b(c)d)e" => "a(b(c)dXe"
replaceNextMatchingClosingBracket :: String -> String -> String
replaceNextMatchingClosingBracket expression replacement = ""



-- Split operands from operators!

splitInput :: String -> ([Operand], [Operator])
splitInput expression = groupOperandsAndOperators $ splitOperandsFromOperators expression

-- Split operands from operators #1
-- "5x`sin`-1`neg`" => ["5x", "`sin`-", "1", "`neg`"]
splitOperandsFromOperators :: String -> [String]
splitOperandsFromOperators input = []

-- Split operands from operators #2
-- ["5x", "`sin`-", "1", "`neg`"] => (["5x", "1"], ["`sin`-", "`neg`"])
groupOperandsAndOperators :: [String] -> ([Operand], [Operator])
groupOperandsAndOperators input = ([], [])



-- Parse operators!

parseOperatorInput :: [String] -> [Operator]
parseOperatorInput groups = parseOperators $ splitOperatorGroups groups

-- Operator parsing #1
-- ["`sin`-", "`neg`"] => ["`sin`", "-", "`neg`"]
splitOperatorGroups :: [String] -> [String]
splitOperatorGroups input = []

-- Operator parsing #2
-- ["`sin`", "-", "`neg`"] => [SINE, SUBTRACT, NEGATIVE]
parseOperators :: [String] -> [Operator]
parseOperators input = []



-- Parse operands!

parseOperandInput :: [String] -> [Operand]
parseOperandInput groups = splitOperandGroups groups

-- Operand parsing #1
-- ["5x", "1"] => [(["5", "x"], [MULTIPLY]), "1"]
splitOperandGroups :: [String] -> [Operand]
splitOperandGroups input = []
