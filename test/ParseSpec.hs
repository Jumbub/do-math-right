module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Parse

import Definitions

groupedTests = [
        ("splitInput", [
            ("", []),
            ("1+2", ["1", "+", "2"]),
            ("123+456", ["123", "+", "456"]),
            ("1+2-3", ["1", "+", "2", "-", "3"]),
            ("1-(1+1)", ["1", "-(", "1", "+", "1", ")"]),
            ("2SIN", ["2", "SIN"]),
            ("1+2SIN/1", ["1", "+", "2", "SIN/", "1"])
        ])
    ]

parseSpec :: IO ()
parseSpec = hspec $ do
    forM_ groupedTests $ \(label, tests) -> do
        describe label $ do
            forM_ tests $ \(input, output) -> do
                it input $ do
                    splitInput input `shouldBe` output

