module ParseSpec (parseSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Parse

parseSpec :: IO ()
parseSpec = hspec $ do
  describe "split operands and operators" $ do

    it "'' => []" $ do
      simplifyInput "" `shouldBe` ""
