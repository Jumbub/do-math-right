module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Either

import Parse
import Operator
import Operand

cleanTests = [
        ("sin( 1x ) + cos (1)", "sin(1x)+cos(1)"),
        ("1 + 1", "1+1"),
        ("", "")
    ]

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
        (
            ["2", "x"],
            [Left $ num 2, Left $ var 'x']
        ),
        (
            ["1"],
            [Left $ num 1]
        ),
        (
            ["+"],
            [Right Addition]
        )
    ]

implicitOperatorTests = [
        (
            "(2x)x",
            [Right LeftParentheses, Left $ num 2, Left $ var 'x', Right RightParentheses, Left $ var 'x'],
            [
                Right LeftParentheses, Left $ num 2, Right Multiplication, Left $ var 'x',
                Right RightParentheses, Right Multiplication, Left $ var 'x'
            ]
        ),
        (
            "2(x)",
            [Left $ num 2, Right LeftParentheses, Left $ var 'x', Right RightParentheses],
            [
                Left $ num 2, Right Multiplication, Right LeftParentheses, Left $ var 'x', Right RightParentheses
            ]
        ),
        (
            "(2)x",
            [Right LeftParentheses, Left $ num 2, Right RightParentheses, Left $ var 'x'],
            [
                Right LeftParentheses, Left $ num 2, Right RightParentheses, Right Multiplication, Left $ var 'x'
            ]
        ),
        (
            "2xy+xy",
            [Left $ num 2, Left $ var 'x', Left $ var 'y', Right Addition, Left $ var 'x', Left $ var 'y'],
            [
                Left $ num 2, Right Multiplication, Left $ var 'x', Right Multiplication, Left $ var 'y',
                Right Addition, Left $ var 'x', Right Multiplication, Left $ var 'y'
            ]
        ),
        (
            "2xy",
            [Left $ num 2, Left $ var 'x', Left $ var 'y'],
            [Left $ num 2, Right Multiplication, Left $ var 'x', Right Multiplication, Left $ var 'y']
        ),
        (
            "2x",
            [Left $ num 2, Left $ var 'x'],
            [Left $ num 2, Right Multiplication, Left $ var 'x']
        ),
        (
            "xy",
            [Left $ var 'x', Left $ var 'y'],
            [Left $ var 'x', Right Multiplication, Left $ var 'y']
        ),
        (
            "x",
            [Left $ var 'x'],
            [Left $ var 'x']
        ),
        (
            "2",
            [Left $ num 2],
            [Left $ num 2])
    ]

parseSpec :: IO ()
parseSpec = hspec $ do
    describe "can clean expression" $ do
        forM_ cleanTests $ \(input, output) -> do
            it ("'" ++ input ++ "'") $ do
                cleanInput input `shouldBe` output
    describe "can split the operands and operators" $ do
        forM_ splitTests $ \(input, output) -> do
            it ("'" ++ input ++ "'") $ do
                splitInput input `shouldBe` output
    describe "can parse the operands and operators" $ do
        forM_ parseTests $ \(input, output) -> do
            it ("'" ++ (concat input) ++ "'") $ do
                parseSplits input `shouldBe` output
    describe "can add any implicit operations" $ do
        forM_ implicitOperatorTests $ \(label, input, output) -> do
            it ("'"++label++"'") $ do
                addImplicitOperations input `shouldBe` output
