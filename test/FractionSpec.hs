module FractionSpec (fractionSpec) where

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad

import Fraction

half = (1, 2)
one = (1, 1)
two = (2, 1)

point1 = (1, 10)
point2 = (1, 5)
point3 = (3, 10)

hard1 = ((37, 84), (29, 78))
hard2 = ((29, 18), (42, 33))

addTests = [
    ((hard1, hard2), ((517,252),(1411,858))),
    (((one, point1), (one, point1)), (two, point2)),
    (((one, point1), fromExact one), (two, point1)),
    ((fromExact one, (one, point1)), (two, point1)),
    ((fromExact one, fromExact one), fromExact two) ]

subtractTests = [
    ((hard1, hard2), ((-295, 252), (1411, 858))),
    (((two, point1), (one, point1)), (one, point2)),
    (((two, point1), fromExact one), (one, point1)),
    ((fromExact two, (one, point1)), (one, point1)),
    ((fromExact two, fromExact one), fromExact one) ]

multiplyTests = [
    ((hard1, hard2), ((255751, 216216), (17909, 15444))),
    (((one, point1), (two, point1)), ((201, 100), point3)),
    (((one, point1), fromExact two), (two, point2)),
    ((fromExact one, (two, point1)), (two, point1)),
    ((fromExact one, fromExact two), fromExact two) ]

divideTests = [
    ((hard1, hard2), ((8439783, 6962774), (590997, 497341))),
    (((one, point1), (two, point1)), ((67, 133), (10, 133))),
    (((one, point1), fromExact two), (half, (1, 20))),
    ((fromExact one, (two, point1)), ((200, 399), (10, 399))),
    ((fromExact one, fromExact two), fromExact half) ]

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
        forM_ divideTests $ \((a, b), c) -> do
            it (show a ++ " / " ++ show b ++ " = " ++ show c) $ do
                Fraction.divide a b `shouldBe` c
