module FractionSpec (fractionSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad

import Fraction

one = (1, 1)
two = (2, 1)

point1 = (1, 10)
point2 = (1, 5)
point3 = (3, 10)

hard1 = PlusOrMinus ((223, 1), (3, 10))
hard2 = PlusOrMinus ((32, 1), (7, 1000))

addTests = [
    ((hard1, hard2), PlusOrMinus ((255, 1), (307, 1000))),
    ((PlusOrMinus (one, point1), PlusOrMinus (one, point1)), PlusOrMinus (two, point2)),
    ((PlusOrMinus (one, point1), PlusOrMinus (one, point1)), PlusOrMinus (two, point2)),
    ((PlusOrMinus (one, point1), Exact one), PlusOrMinus (two, point1)),
    ((Exact one, PlusOrMinus (one, point1)), PlusOrMinus (two, point1)),
    ((Exact one, Exact one), Exact two) ]

subtractTests = [
    ((hard1, hard2), PlusOrMinus ((191, 1), (307, 1000))),
    ((PlusOrMinus (two, point1), PlusOrMinus (one, point1)), PlusOrMinus (one, point2)),
    ((PlusOrMinus (two, point1), Exact one), PlusOrMinus (one, point1)),
    ((Exact two, PlusOrMinus (one, point1)), PlusOrMinus (one, point1)),
    ((Exact two, Exact one), Exact one) ]

multiplyTests = [
    ((hard1, hard2), PlusOrMinus ((7136, 1), (11161, 1000))),
    ((PlusOrMinus (two, point1), PlusOrMinus (one, point1)), PlusOrMinus (two, point3)),
    ((PlusOrMinus (two, point1), Exact one), PlusOrMinus (two, point1)),
    ((Exact two, PlusOrMinus (one, point1)), PlusOrMinus (two, point1)),
    ((Exact two, Exact one), Exact two) ]

fractionSpec :: IO ()
fractionSpec = hspec $ do
    describe "operations on fractions with accuracies" $ do
        forM_ addTests $ \((a, b), c) -> do
            it (show a ++ " + " ++ show b ++ " = " ++ show c) $ do
                Fraction.add a b `shouldBe` c
        forM_ subtractTests $ \((a, b), c) -> do
            it (show a ++ " - " ++ show b ++ " = " ++ show c) $ do
                Fraction.subtract a b `shouldBe` c
        forM_ multiplyTests $ \((a, b), c) -> do
            it (show a ++ " * " ++ show b ++ " = " ++ show c) $ do
                Fraction.multiply a b `shouldBe` c