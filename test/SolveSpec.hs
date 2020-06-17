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
            "1+1",
            ([num 1, num 1], Addition),
            [num 2]
        )
    ]

solveSpec :: IO ()
solveSpec = hspec $ do
    describe "can perform operations" $ do
        forM_ operationTests $ \(label, (operandStack, operation), output) -> do
            it ("'" ++ label ++ "'") $ do
                performOperation operation operandStack `shouldBe` output
