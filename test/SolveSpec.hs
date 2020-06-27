module SolveSpec (solveSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Either

import Solve
import Operand
import Operator
import Type

operationTests = [
        (
            ([num 1, num 1], Addition),
            [num 2]
        )
    ]

solveTests = [
        ("xy", "xy"),
        ("1-(-1)", "2"),
        ("0.125 + 0.125", "1/4"),
        ("0.124", "31/250"),
        ("2+((4+8)*16/(32-64))", "-4"),
        ("8*(4+2)", "48"),
        ("(2+4)*8", "48"),
        ("1+2-3*4/5", "3/5"),
        ("8*4+2", "34"),
        ("2+4*8", "34"),
        ("2/10", "1/5"),
        ("10/2", "5"),
        ("2*3", "6"),
        ("1-1", "0"),
        ("1+1", "2"),
        ("1", "1")
    ]

operandsToString :: Context -> [Operand] -> String
operandsToString context operands = show (map (operandToString context) operands)

solveSpec :: IO ()
solveSpec = hspec $ do
    let ctx = defaultContext
    describe "can perform operations (default context)" $ do
        forM_ operationTests $ \((operands, operation), output) -> do
            it ("'" ++ operandsToString ctx operands ++ ", " ++ (show operation) ++ "'") $ do
                let (ctx', actual) = performOperation ctx operation operands
                actual `shouldBe` output
                ctx' `shouldBe` ctx
    describe "can solve expressions (default context)" $ do
        forM_ solveTests $ \(input, output) -> do
            it (input ++ " => " ++ output) $ do
                let (ctx', actual) = solve ctx input
                actual `shouldBe` output
                ctx' `shouldBe` ctx
