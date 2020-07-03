module DecimalSpec (decimalSpec, decimalToString) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Either

import Decimal
import Type

fractionToDecimalTests = [
        ((1, 1000000, PlusOrMinus (1, 1000000)), PlusOrMinusDecimal (0, [0,0,0,0,0,5], (3, 500000))),
        ((1, 1000000, PlusOrMinus (1, 1)), PlusOrMinusDecimal (0, [0,0,0,0,0,5], (200001, 200000))),
        ((1, 100000, PlusOrMinus (1, 1)), PlusOrMinusDecimal (0, [0,0,0,0,1], (1, 1))),
        ((1, 1, PlusOrMinus (1, 1)), PlusOrMinusDecimal (1, [], (1, 1))),
        ((1, 128, Exact), PlusOrMinusDecimal (0, [0,0,7,8,1], (1, 100000))),
        ((1, 21, Exact), PlusOrMinusDecimal (0, [0,4,7,6,1], (1, 100000))),
        ((1, 1000000, Exact), PlusOrMinusDecimal (0, [0,0,0,0,0], (1, 100000))),
        ((7, 12, Exact), RecurringDecimal (0, [5,8], [3])),
        ((9, 11, Exact), RecurringDecimal (0, [], [8,1])),
        ((10, 3, Exact), RecurringDecimal (3, [], [3])),
        ((4, 2, Exact), ExactDecimal (2, [])),
        ((2, 2, Exact), ExactDecimal (1, [])),
        ((1, 4, Exact), ExactDecimal (0, [2,5])),
        ((1, 2, Exact), ExactDecimal (0, [5])),
        ((1, 1, Exact), ExactDecimal (1, []))
    ]

decimalToStringTests = [
        (PlusOrMinusDecimal (12, [34], (1, 1000)), "12.34 ± 1/1000"),
        (PlusOrMinusDecimal (12, [], (1, 1000)), "12 ± 1/1000"),
        (RecurringDecimal (12, [34], []), "12.34"),
        (RecurringDecimal (12, [34], [56]), "12.34(56)"),
        (RecurringDecimal (12, [], [34]), "12.(34)"),
        (RecurringDecimal (12, [34], []), "12.34"),
        (RecurringDecimal (12, [], []), "12"),
        (ExactDecimal (12, [34]), "12.34"),
        (ExactDecimal (12, []), "12")
    ]

decimalSpec :: IO ()
decimalSpec = hspec $ do
    let context = defaultContext {accuracy = Exact, maxDecimalCalculations = 10, maxDisplayedDecimals = 5}
    describe "fraction to decimal" $ do
        forM_ fractionToDecimalTests $ \(input, output) -> do
            it (show input ++ " => " ++ show output) $ do
                (fractionToDecimal context input) `shouldBe` output
    describe "decimal to string" $ do
        forM_ decimalToStringTests $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                decimalToString input `shouldBe` output
