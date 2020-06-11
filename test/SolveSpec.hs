module SolveSpec (solveSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Solve

solveSpec :: IO ()
solveSpec = hspec $ do
  describe "literals" $ do

    it "0 => 0" $ do
      solve "0" [] `shouldBe` ""

    -- it "159 => 159" $ do
    --   solve "159" [] `shouldBe` "159"

    -- it "-159 => -159" $ do
    --   solve "-159" [] `shouldBe` "-159"

  -- describe "multiple literals" $ do

  --   it " => Error" $ do
  --     solve "" `shouldBe` "Bad equation: no return values"

  --   it "159 -159 => Error" $ do
  --     solve "159 -159" `shouldBe` "Bad equation: multiple return values"

  --   it "159 -159 0 951 => Error" $ do
  --     solve "159 -159 0 951" `shouldBe` "Bad equation: multiple return values"

  -- describe "literal addition" $ do

  --   it "1 + 1 => 2" $ do
  --     solve "1 + 1" `shouldBe` "2"
