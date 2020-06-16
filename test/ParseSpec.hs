module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad

import Parse
import Operator
import Operand

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
            [Right Sine, Right LeftParentheses, Left $ num 1, Right Addition, Left $ num 5, Right RightParentheses]
        ),
        (["2", "x"], [Left $ num 2, Left $ var 'x']),
        (["1"], [Left $ num 1]),
        (["+"], [Right Addition])
    ]

implicitOperatorTests = [
        ([Left $ num 2, Left $ var 'x', Left $ var 'y'], [Left $ num 2, Right Multiplication, Left $ var 'x', Right Multiplication, Left $ var 'y']),
        ([Left $ num 2, Left $ var 'x'], [Left $ num 2, Right Multiplication, Left $ var 'x']),
        ([Left $ var 'x', Left $ var 'y'], [Left $ var 'x', Right Multiplication, Left $ var 'y']),
        ([Left $ var 'x', Left $ var 'y'], [Left $ var 'x', Right Multiplication, Left $ var 'y'])
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
    describe "can add any implicit operations" $ do
        forM_ implicitOperatorTests $ \(input, output) -> do
            -- TODO: figure out how to conver this input to a string
            it ("''") $ do
                addImplicitOperations input `shouldBe` output
