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

fracCtx = defaultContext { fractionResult = True }
decCtx = defaultContext { fractionResult = False, decimalPlaces = 5 }

same :: String -> [(String, Context, String)]
same input = [("as fraction", fracCtx, input), ("as decimal", decCtx, input)]

diff :: String -> String -> [(String, Context, String)]
diff a b = [("as fraction", fracCtx, a), ("as decimal", decCtx, b)]

solveTests = [
        ("0.000001", diff "1/1000000" "0.000005 ± 1/200000"),
        ("22/7", diff "22/7" "3.142855 ± 1/200000"),
        ("10/3", diff "10/3" "3.(3)"),
        ("xy", same "xy"),
        ("1-(-1)", same "2"),
        ("0.125 + 0.125", diff "1/4" "0.25"),
        ("0.124", diff "31/250" "0.124"),
        ("2+((4+8)*16/(32-64))", same "-4"),
        ("8*(4+2)", same "48"),
        ("(2+4)*8", same "48"),
        ("1+2-3*4/5", diff "3/5" "0.6"),
        ("8*4+2", same "34"),
        ("2+4*8", same "34"),
        ("2/10", diff "1/5" "0.2"),
        ("10/2", same "5"),
        ("2*3", same "6"),
        ("1-1", same "0"),
        ("1+1", same "2"),
        ("1", same "1")
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
    describe "can solve expressions" $ do
        forM_ solveTests $ \(input, outputs) -> do
            forM_ outputs $ \(label, ctx, output) -> do
                it (input ++ " => " ++ output ++ " (" ++ label ++ ")") $ do
                    let (ctx', actual) = solve ctx input
                    actual `shouldBe` output
                    ctx' `shouldBe` ctx
