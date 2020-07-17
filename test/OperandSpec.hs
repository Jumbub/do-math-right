module OperandSpec (operandSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Either

import Operand
import Context

stringify = [
        (var 'x', "x"),
        (frac 10 3, "10/3"),
        (frac 1 256, "1/256"),
        (frac 1 2, "1/2"),
        (num 1, "1")
    ]

operandify :: [(String, Maybe Operand)]
operandify = [
        ("000123.000456000", Just $ frac 15375057 125000),
        ("123.000456", Just $ frac 15375057 125000),
        ("1.0", Just $ num 1),
        ("1", Just $ num 1)
    ]

ctx :: Context
ctx = defaultContext { fractionResult = True }

operandSpec :: IO ()
operandSpec = hspec $ do
    describe "can convert operand to string" $ do
        forM_ stringify $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                operandToString ctx input `shouldBe` output
    describe "can convert string to operand" $ do
        forM_ operandify $ \(input, output) -> do
            it (input ++ " => " ++ show output) $ do
                stringToOperand input `shouldBe` output
