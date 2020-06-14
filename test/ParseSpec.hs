module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Parse

import Definitions

parseSpec :: IO ()
parseSpec = hspec $ do
  describe "splitInput" $ do

    it "'' => []" $ do
      splitInput "" `shouldBe` []

    it "'123' => []" $ do
      splitInput "123" `shouldBe` ["123"]

    it "'1+2' => []" $ do
      splitInput "1+2" `shouldBe` ["1", "+", "2"]

    it "'123+456' => []" $ do
      splitInput "123+456" `shouldBe` ["123", "+", "456"]

    it "'1+2-3' => []" $ do
      splitInput "1+2-3" `shouldBe` ["1", "+", "2", "-", "3"]

    it "'2`sin`' => []" $ do
      splitInput "2SIN" `shouldBe` ["2", "SIN"]

    it "'1-(1+1)' => []" $ do
      splitInput "1-(1+1)" `shouldBe` ["1", "-", "(", "1", "+", "1", ")"]
