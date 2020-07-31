module DecimalSpec (decimalSpec, decimalToString) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Either

import Decimal
import Context
import Fraction

fractionToDecimalTests = [
        (((1, 1000000), (1, 1000000)), PlusOrMinusDecimal (Positive, 0, [], (11, 1000000))),
        (((1, 1000000), (1, 1)), PlusOrMinusDecimal (Positive, 0, [], (100001, 100000))),
        (((1, 100000), (1, 1)), PlusOrMinusDecimal (Positive, 0, [0,0,0,0,1], (1, 1))),
        (((1, 1), (1, 1)), PlusOrMinusDecimal (Positive, 1, [], (1, 1))),
        ((fromExact(1, 128)), PlusOrMinusDecimal (Positive, 0, [0,0,7,8,1], (1, 100000))),
        ((fromExact(1, 21)), PlusOrMinusDecimal (Positive, 0, [0,4,7,6,1], (1, 100000))),
        ((fromExact (1, 1000000)), PlusOrMinusDecimal (Positive, 0, [], (1, 100000))),
        ((fromExact (7, 12)), RecurringDecimal (Positive, 0, [5,8], [3])),
        ((fromExact (9, 11)), RecurringDecimal (Positive, 0, [], [8,1])),
        ((fromExact (10, 3)), RecurringDecimal (Positive, 3, [], [3])),
        ((fromExact (4, 2)), ExactDecimal (Positive, 2, [])),
        ((fromExact (2, 2)), ExactDecimal (Positive, 1, [])),
        ((fromExact (1, 4)), ExactDecimal (Positive, 0, [2,5])),
        ((fromExact (1, 2)), ExactDecimal (Positive, 0, [5])),
        ((fromExact (1, 1)), ExactDecimal (Positive, 1, []))
    ]

decimalToStringTests = [
        (PlusOrMinusDecimal (Positive, 12, [3,4], (123, 321)), "12.34 ± 123/321"),
        (PlusOrMinusDecimal (Positive, 12, [3,4], (1, 1000)), "12.34 ± 1/1000"),
        (PlusOrMinusDecimal (Positive, 12, [], (1, 1000)), "12 ± 1/1000"),
        (RecurringDecimal (Positive, 12, [3,4], []), "12.34"),
        (RecurringDecimal (Positive, 12, [3,4], [5,6]), "12.34(56)"),
        (RecurringDecimal (Positive, 12, [], [3,4]), "12.(34)"),
        (RecurringDecimal (Positive, 12, [3,4], []), "12.34"),
        (RecurringDecimal (Positive, 12, [], []), "12"),
        (ExactDecimal (Positive, 12, [3,4]), "12.34"),
        (ExactDecimal (Positive, 12, []), "12")
    ]

fullTests = [
        (((-1, 10), (5, 1)), "-0.1 ± 5"),
        (((-1, 10), (1, 1)), "-0.1 ± 1"),
        (((3141592, 1000000), (1, 1)), "3.14159 ± 100001/100000"),
        (((3141592, 1000000), (1, 1000000)), "3.14159 ± 11/1000000"),
        (((314159,  100000),  (1, 100000)),  "3.14159 ± 1/100000"),
        (((31415,   10000),   (1, 10000)),   "3.1415 ± 1/10000"),
        (fromExact (22, 7), "3.14285 ± 1/100000"),
        (fromExact (1, 3), "0.(3)"),
        (fromExact (1, 1), "1")
    ]

decimalSpec :: IO ()
decimalSpec = hspec $ do
    let context = defaultContext {decimalResult = True, internalDecimalPlaces = 5}
    describe "fraction to decimal" $ do
        forM_ fractionToDecimalTests $ \(input, output) -> do
            it (show input ++ " => " ++ show output) $ do
                (fractionToDecimal context input) `shouldBe` output
    describe "decimal to string" $ do
        forM_ decimalToStringTests $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                decimalToString input `shouldBe` output
    describe "fraction to decimal to string" $ do
        forM_ fullTests $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                (decimalToString $ fractionToDecimal context input) `shouldBe` output
