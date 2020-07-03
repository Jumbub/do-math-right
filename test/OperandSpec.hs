module OperandSpec (operandSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Either

import Operand
import Type

stringify = [
        (var 'x', "x"),
        (frac 10 3, "10/3"),
        (frac 1 256, "1/256"),
        (frac 1 2, "1/2"),
        (num 1, "1")
    ]

operandSpec :: IO ()
operandSpec = hspec $ do
    describe "can convert operand to string" $ do
        forM_ stringify $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                operandToString defaultContext input `shouldBe` output
