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
    Fraction.mod,
) where

import Data.Sort

import FractionType
import ExactFraction
import Irrational

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
sin dp x = (result, requiredAccuracy)
    where
        x' = Fraction.mod x (Fraction.multiply (Fraction.fromInteger 2) $ rationalise dp Pi)
        (result, _) = sin' requiredAccuracy x' x' True x' x' 1 1
        requiredAccuracy = (1, 10 ^ dp)
        sin' :: ExactFraction -> Fraction -> Fraction -> Bool -> Fraction -> Fraction -> Integer -> Integer -> Fraction
        sin' required acc@(xn, xp) lastVal lastAdd lastNum x lastDen lastFact
            | ExactFraction.compare accuracy required == LT = (xn, finalAccuracy)
            | otherwise = sin' required acc' currentVal (not lastAdd) num x den (lastFact + 2)
            where
                acc' = operator acc currentVal
                operator = if (lastAdd) then Fraction.subtract else Fraction.add
                currentVal = Fraction.divide num ((den, 1), (0, 1))
                num = Fraction.multiply x $ Fraction.multiply x lastNum
                den = lastDen * (lastFact + 1) * (lastFact + 2)
                accuracy = absBiggestExact lastVal
                finalAccuracy = ExactFraction.add xp accuracy
