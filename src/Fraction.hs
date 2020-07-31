module Fraction (
    Fraction,
    Fraction.fromExact,
    Fraction.fromInteger,
    Fraction.simplify,
    Fraction.flipSign,
    Fraction.reciprocal,
    Fraction.add,
    Fraction.subtract,
    Fraction.multiply,
    Fraction.divide,
    Fraction.sin,
    Fraction.cos,
    Fraction.tan,
    Fraction.mod,
) where

import Data.Sort
import Debug.Trace

import FractionType
import ExactFraction
import Irrational
import Utility

fromInteger :: Integer -> Fraction
fromInteger x = fromExact (x, 1)

fromExact :: ExactFraction -> Fraction
fromExact a = (a, (0, 1))

normalise :: Fraction -> Fraction
normalise ((0, _), _) = ((0, 1), (0, 1))
normalise (a, (0, _)) = (a, (0, 1))

simplify :: Fraction -> Fraction
simplify ((0, _), ap) = ((0, 1), (0, 1))
simplify (a, ap) = normalise (ExactFraction.simplify a, ExactFraction.simplify ap)

absolute :: Fraction -> Fraction
absolute (a, ap) = (ExactFraction.absolute a, ap)

flipSign :: Fraction -> Fraction
flipSign (a, ap) = (ExactFraction.flipSign a, ap)

reciprocal :: Fraction -> Fraction
reciprocal ((0, d), _) = ((0, 1), (0, 1))
reciprocal ((n, d), (0, pd)) = ((d, n), (0, 1))
reciprocal ((n, d), p) = ((d, n), p)

add :: Fraction -> Fraction -> Fraction
add (a, ap) (b, bp) = (ExactFraction.add a b, ExactFraction.add ap bp)

subtract :: Fraction -> Fraction -> Fraction
subtract a b = Fraction.add a (Fraction.flipSign b)

operateOnFractionABSimple :: (ExactFraction -> ExactFraction -> ExactFraction) -> Fraction -> Fraction -> Fraction
operateOnFractionABSimple operation (a, ap) (b, bp) = (output, plusOrMinus)
    where
        w = operation (ExactFraction.add a ap) (ExactFraction.add b bp)
        x = operation (ExactFraction.add a ap) (ExactFraction.subtract b bp)
        y = operation (ExactFraction.subtract a ap) (ExactFraction.add b bp)
        z = operation (ExactFraction.subtract a ap) (ExactFraction.subtract b bp)
        -- ordered = sortBy ExactFraction.compare [w, x, y, z]
        -- minN = head ordered
        -- maxN = last ordered
        -- output = operation a b
        -- a = ExactFraction.absolute (ExactFraction.subtract output minN)
        -- b = ExactFraction.absolute (ExactFraction.subtract output maxN)
        -- ordered' = sortBy ExactFraction.compare [a, b]
        -- plusOrMinus = ExactFraction.divide (last ordered') (ExactFraction.fromInteger 2)
        ordered = sortBy ExactFraction.compare [w, x, y, z]
        minN = head ordered
        maxN = last ordered
        output = operation a b
        minD = ExactFraction.absolute (ExactFraction.subtract output minN)
        maxD = ExactFraction.absolute (ExactFraction.subtract output maxN)
        ordered' = sortBy ExactFraction.compare [minD, maxD]
        plusOrMinus = ExactFraction.divide (last ordered') (ExactFraction.fromInteger 2)

operateOnFractionAB :: (ExactFraction -> ExactFraction -> ExactFraction) -> Fraction -> Fraction -> Fraction
operateOnFractionAB operation (a, ap) (b, bp) = (output, plusOrMinus)
    where
        w = operation (ExactFraction.add a ap) (ExactFraction.add b bp)
        x = operation (ExactFraction.add a ap) (ExactFraction.subtract b bp)
        y = operation (ExactFraction.subtract a ap) (ExactFraction.add b bp)
        z = operation (ExactFraction.subtract a ap) (ExactFraction.subtract b bp)
        ordered = sortBy ExactFraction.compare [w, x, y, z]
        minN = head ordered
        maxN = last ordered
        dist = ExactFraction.absolute (ExactFraction.subtract maxN minN)
        plusOrMinus = ExactFraction.divide dist (ExactFraction.fromInteger 2)
        output = ExactFraction.add minN plusOrMinus

operateOnFractionA :: (ExactFraction -> ExactFraction) -> Fraction -> Fraction
operateOnFractionA operation (a, ap) = (output, plusOrMinus)
    where
        x = operation (ExactFraction.add a ap)
        y = operation (ExactFraction.subtract a ap)
        ordered = sortBy ExactFraction.compare [x, y]
        minN = head ordered
        maxN = last ordered
        dist = ExactFraction.absolute (ExactFraction.subtract maxN minN)
        plusOrMinus = ExactFraction.divide dist (ExactFraction.fromInteger 2)
        output = ExactFraction.add minN plusOrMinus

multiply :: Fraction -> Fraction -> Fraction
multiply a b = operateOnFractionAB ExactFraction.multiply a b

divide :: Fraction -> Fraction -> Fraction
divide a b = operateOnFractionAB ExactFraction.divide a b

powerN :: Integer -> Fraction -> Fraction
powerN n a = operateOnFractionA (ExactFraction.powerN n) a

absBiggestExact :: Fraction -> ExactFraction
absBiggestExact (a@(n, d), ap)
    | n < 0 = ExactFraction.add (-n, d) ap
    | otherwise = ExactFraction.add a ap

mod :: Fraction -> Fraction -> Fraction
mod x m = operateOnFractionAB ExactFraction.mod x m

sin :: Integer -> Fraction -> Fraction
sin idp x = sin' (1, 10 ^ idp) x 1
    where
        -- x = x'
        -- req = required accuracy
        -- sum = summation
        -- n = depth of summation
        sin' :: ExactFraction -> Fraction -> Integer -> Fraction
        sin' req sum n
            | ExactFraction.compare fracAccuracy req /= GT = newSum
            | otherwise = sin' req newSum (n + 1)
            where
                xpowd = Fraction.powerN (2 * n + 1) x
                sign = (-1) ^ n
                num = Fraction.multiply (Fraction.fromInteger sign) xpowd
                den = factorial (2 * n + 1)
                frac@(frace, fracp) = Fraction.divide num (Fraction.fromInteger den)
                fracAccuracy = ExactFraction.add (ExactFraction.absolute frace) fracp
                newSum = Fraction.add sum frac

cos :: Integer -> Fraction -> Fraction
cos dp x = Fraction.sin dp (Fraction.add x $ Fraction.divide (rationalise dp Pi) (Fraction.fromInteger 2))

tan :: Integer -> Fraction -> Fraction
tan dp x = Fraction.divide (Fraction.sin dp x) (Fraction.cos dp x)
