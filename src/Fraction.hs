module Fraction (
    Fraction(..),
    Fraction.simplify,
    Fraction.flipSign,
    Fraction.reciprocal,
    Fraction.add,
    Fraction.subtract,
    Fraction.multiply,
    Fraction.divide,
    Fraction.fromExact,
) where

import Data.Sort

import ExactFraction

type Fraction = (ExactFraction, ExactFraction)

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

multiply :: Fraction -> Fraction -> Fraction
multiply (a, ap) (b, bp) = (output, plusOrMinus)
    where
        w = ExactFraction.multiply (ExactFraction.add a ap) (ExactFraction.add b bp)
        x = ExactFraction.multiply (ExactFraction.add a ap) (ExactFraction.subtract b bp)
        y = ExactFraction.multiply (ExactFraction.subtract a ap) (ExactFraction.add b bp)
        z = ExactFraction.multiply (ExactFraction.subtract a ap) (ExactFraction.subtract b bp)
        ordered = sortBy ExactFraction.compare [w, x, y, z]
        minN = head ordered
        maxN = last ordered
        dist = ExactFraction.absolute (ExactFraction.subtract maxN minN)
        plusOrMinus = ExactFraction.divide dist (fromNumber 2)
        output = ExactFraction.add minN plusOrMinus

divide :: Fraction -> Fraction -> Fraction
divide (a, ap) (b, bp) = (output, plusOrMinus)
    where
        w = ExactFraction.divide (ExactFraction.add a ap) (ExactFraction.add b bp)
        x = ExactFraction.divide (ExactFraction.add a ap) (ExactFraction.subtract b bp)
        y = ExactFraction.divide (ExactFraction.subtract a ap) (ExactFraction.add b bp)
        z = ExactFraction.divide (ExactFraction.subtract a ap) (ExactFraction.subtract b bp)
        ordered = sortBy ExactFraction.compare [w, x, y, z]
        minN = head ordered
        maxN = last ordered
        dist = ExactFraction.absolute (ExactFraction.subtract maxN minN)
        plusOrMinus = ExactFraction.divide dist (fromNumber 2)
        output = ExactFraction.add minN plusOrMinus