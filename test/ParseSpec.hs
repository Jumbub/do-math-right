module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Parse

import Definitions

groupedTests = [
        ("can split the operands and operators", splitInput, [
            ("1y+SIN(2/32.54)/z", ["1", "y", "+", "SIN", "(", "2", "/", "32.54", ")", "/", "z"]),
            ("SIN(2x)", ["SIN", "(", "2", "x", ")"]),
            ("1-((-1)+1)", ["1", "-", "(", "(", "-", "1", ")", "+", "1", ")"]),
            ("123+456", ["123", "+", "456"]),
            ("123.456", ["123.456"]),
            ("PI", ["PI"]),
            ("x", ["x"]),
            ("1", ["1"]),
            ("", [])
        ])
        -- ("can parse the operands and operators", parseInput, [
        --     (["SIN", "(", "1", "+", "5", ")"], [SINE, LEFT_PARENTHESES, ([1], []), ADDITION, ([5], []), RIGHT_PARENTHESES]),
        --     (["+"], [ADDITION]),
        --     (["2", "x"], [(['x'], []), ([1], [])])
        -- ])
    ]

parseSpec :: IO ()
parseSpec = hspec $ do
    forM_ groupedTests $ \(label, func, tests) -> do
        describe label $ do
            forM_ tests $ \(input, output) -> do
                it ("'" ++ input ++ "'") $ do
                    func input `shouldBe` output

