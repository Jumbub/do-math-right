import Test.Hspec
import Control.Exception (evaluate)
import Solve

main :: IO ()
main = hspec $ do
  describe "literals" $ do
    it "0 = 0" $ do
      solve "0" `shouldBe` "0"

    it "159 = 159" $
      solve "159" `shouldBe` "159"

    it "-159 = -159" $ do
      solve "-159" `shouldBe` "-159"
