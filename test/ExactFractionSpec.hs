module ExactFractionSpec (exactFractionSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Tuple
import Data.Either

import ExactFraction

addTests = [
    (((2, 3), (-3, 5)), (1, 15)),
    (((2, 3), (3, 5)), (19, 15)),
    (((1, 3), (1, 5)), (8, 15)),
    (((1, 4), (1, 4)), (1, 2)),
    (((1, 2), (1, 2)), (1, 1)),
    (((-1, 1), (-1, 1)), (-2, 1)),
    (((1, 1), (1, 1)), (2, 1)) ]

subtractTests = [
    (((2, 3), (-3, 5)), (19, 15)),
    (((2, 3), (3, 5)), (1, 15)),
    (((1, 3), (1, 5)), (2, 15)),
    (((1, 4), (1, 4)), (0, 1)),
    (((1, 2), (1, 2)), (0, 1)),
    (((-1, 1), (-1, 1)), (0, 1)),
    (((1, 1), (1, 1)), (0, 1)) ]

multiplyTests = [
    (((5, 7), (9, 11)), (45, 77)),
    (((-1, 1), (-1, 1)), (1, 1)),
    (((1, 1), (1, 1)), (1, 1)) ]

divideTests = [
    (((1, 4), (1, 4)), (1, 1)),
    (((4, 1), (2, 1)), (2, 1)),
    (((-1, 1), (-1, 1)), (1, 1)),
    (((1, 1), (1, 1)), (1, 1)) ]

exactFractionSpec :: IO ()
exactFractionSpec = hspec $ do
    describe "operations on exact fractions" $ do
        forM_ addTests $ \((a, b), c) -> do
            it (show a ++ " + " ++ show b ++ " = " ++ show c) $ do
                ExactFraction.add a b `shouldBe` c
        forM_ subtractTests $ \((a, b), c) -> do
            it (show a ++ " - " ++ show b ++ " = " ++ show c) $ do
                ExactFraction.subtract a b `shouldBe` c
        forM_ multiplyTests $ \((a, b), c) -> do
            it (show a ++ " * " ++ show b ++ " = " ++ show c) $ do
                ExactFraction.multiply a b `shouldBe` c
        forM_ divideTests $ \((a, b), c) -> do
            it (show a ++ " / " ++ show b ++ " = " ++ show c) $ do
                ExactFraction.divide a b `shouldBe` c