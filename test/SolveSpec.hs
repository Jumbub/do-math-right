module SolveSpec (solveSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Either

import Solve
import Operand
import Operator

operationTests = [
        (
            ([num 1, num 1], Addition),
            [num 2]
        )
    ]

solveTests = [
        ("2+((4+8)*16/(32-64))", "-4"),
        ("8*(4+2)", "48"),
        ("(2+4)*8", "48"),
        ("1+2-3*4/5", "0.6"),
        ("8*4+2", "34"),
        ("2+4*8", "34"),
        ("2/10", "0.2"),
        ("10/2", "5"),
        ("2*3", "6"),
        ("1-1", "0"),
        ("1+1", "2"),
        ("1", "1")
    ]

operandsToString :: [Operand] -> String
operandsToString operands = show (map operandToString operands)

solveSpec :: IO ()
solveSpec = hspec $ do
    describe "can perform operations" $ do
        forM_ operationTests $ \((operands, operation), output) -> do
            it ("'" ++ operandsToString operands ++ ", " ++ (show operation) ++ "'") $ do
                performOperation operation operands `shouldBe` output
    describe "can solve expressions" $ do
        forM_ solveTests $ \(input, output) -> do
            it (input ++ " => " ++ output) $ do
                solve input `shouldBe` output
