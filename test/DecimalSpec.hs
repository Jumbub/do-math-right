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
        (((1, 1000000), (1, 1000000)), PlusOrMinusDecimal (0, [0,0,0,0,0,5], (3, 500000))),
        (((1, 1000000), (1, 1)), PlusOrMinusDecimal (0, [0,0,0,0,0,5], (200001, 200000))),
        (((1, 100000), (1, 1)), PlusOrMinusDecimal (0, [0,0,0,0,1], (1, 1))),
        (((1, 1), (1, 1)), PlusOrMinusDecimal (1, [], (1, 1))),
        ((fromExact(1, 128)), PlusOrMinusDecimal (0, [0,0,7,8,1,5], (1, 200000))),
        ((fromExact(1, 21)), PlusOrMinusDecimal (0, [0,4,7,6,1,5], (1, 200000))),
        ((fromExact (1, 1000000)), PlusOrMinusDecimal (0, [0,0,0,0,0,5], (1, 200000))),
        ((fromExact (7, 12)), RecurringDecimal (0, [5,8], [3])),
        ((fromExact (9, 11)), RecurringDecimal (0, [], [8,1])),
        ((fromExact (10, 3)), RecurringDecimal (3, [], [3])),
        ((fromExact (4, 2)), ExactDecimal (2, [])),
        ((fromExact (2, 2)), ExactDecimal (1, [])),
        ((fromExact (1, 4)), ExactDecimal (0, [2,5])),
        ((fromExact (1, 2)), ExactDecimal (0, [5])),
        ((fromExact (1, 1)), ExactDecimal (1, []))
    ]

decimalToStringTests = [
        (PlusOrMinusDecimal (12, [34], (123, 321)), "12.34 ± 123/321"),
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

fullTests = [
        (fromExact (22, 7), "3.142855 ± 1/200000"),
        (fromExact (1, 3), "0.(3)"),
        (fromExact (1, 1), "1")
    ]

decimalSpec :: IO ()
decimalSpec = hspec $ do
    let context = defaultContext {fractionResult = False, decimalPlaces = 5}
    describe "fraction to decimal" $ do
        forM_ fractionToDecimalTests $ \(input, output) -> do
            it (show input ++ " => " ++ show output) $ do
                (fractionToDecimal context input) `shouldBe` output
    describe "decimal to string" $ do
        forM_ decimalToStringTests $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                decimalToString input `shouldBe` output
    describe "fraction to decimal string" $ do
        forM_ fullTests $ \(input, output) -> do
            it (show input ++ " => " ++ output) $ do
                (decimalToString $ fractionToDecimal context input) `shouldBe` output
