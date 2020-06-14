module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Parse

import Definitions

groupedTests = [
        ("can split the operands and operators", splitInput, [
            ("", []),
            ("1", ["1"]),
            ("x", ["x"]),
            ("PI", ["PI"]),
            ("123+456", ["123", "+", "456"]),
            ("1-((-1)+1)", ["1", "-", "(", "(", "-", "1", ")", "+", "1", ")"]),
            ("SIN(2x)", ["SIN", "(", "2", "x", ")"]),
            ("1y+SIN(2/2)/z", ["1", "y", "+", "SIN", "(", "2", "/", "2", ")", "/", "z"])
        ])
    ]

parseSpec :: IO ()
parseSpec = hspec $ do
    forM_ groupedTests $ \(label, func, tests) -> do
        describe label $ do
            forM_ tests $ \(input, output) -> do
                it ("'" ++ input ++ "'") $ do
                    func input `shouldBe` output

