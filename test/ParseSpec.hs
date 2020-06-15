module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad

import Parse
import Operator

splitTests = [
        ("1y+SIN(2/32.54)/z", ["1", "y", "+", "SIN", "(", "2", "/", "32", ".", "54", ")", "/", "z"]),
        ("SIN(2x)", ["SIN", "(", "2", "x", ")"]),
        ("1-((-1)+1)", ["1", "-", "(", "(", "-", "1", ")", "+", "1", ")"]),
        ("123+456", ["123", "+", "456"]),
        ("123.456", ["123", ".", "456"]),
        ("PI", ["PI"]),
        ("x", ["x"]),
        ("1", ["1"]),
        ("", [])
    ]

parseTests = [
        (
            ["SIN", "(", "1", "+", "5", ")"],
            [Right Sine, Right LeftParentheses, Left ((1, 1), ([], [])), Right Addition,
                Left ((5, 1), ([], [])), Right RightParentheses]
        ),
        (["2", "x"], [Left ((2, 1), ([], [])), Left ((1, 1), (['x'], []))]),
        (["1"], [Left ((1, 1), ([], []))]),
        (["+"], [Right Addition])
    ]

parseSpec :: IO ()
parseSpec = hspec $ do
    describe "can split the operands and operators" $ do
        forM_ splitTests $ \(input, output) -> do
            it ("'" ++ input ++ "'") $ do
                splitInput input `shouldBe` output
    describe "can parse the operands and operators" $ do
        forM_ parseTests $ \(input, output) -> do
            it ("'" ++ (concat input) ++ "'") $ do
                parseSplits input `shouldBe` output
