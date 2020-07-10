module Fraction (
    Fraction(..),
    Fraction.simplify,
    Fraction.flipSign,
    Fraction.reciprocal,
    Fraction.add,
    Fraction.subtract,
    Fraction.multiply,
    Fraction.divide,
) where

import ExactFraction

data Fraction =
    Exact ExactFraction |
    PlusOrMinus (ExactFraction, ExactFraction)
    deriving (Show, Eq)

simplify :: Fraction -> Fraction
simplify (Exact a) = Exact $ ExactFraction.simplify a
simplify (PlusOrMinus (a, ap)) = PlusOrMinus (ExactFraction.simplify a, ExactFraction.simplify ap)

flipSign :: Fraction -> Fraction
flipSign x = Fraction.multiply x (Exact (-1, 1))

reciprocal :: Fraction -> Fraction
reciprocal (Exact (n, d)) = Exact (d, n)
reciprocal (PlusOrMinus ((n, d), x)) = PlusOrMinus ((d, n), x)

add :: Fraction -> Fraction -> Fraction
add (Exact a) (Exact b) = Exact (ExactFraction.add a b)
add (PlusOrMinus (a, ap)) (Exact b) = PlusOrMinus (ExactFraction.add a b, ap)
add (Exact a) (PlusOrMinus (b, bp)) = PlusOrMinus (ExactFraction.add a b, bp)
add (PlusOrMinus (a, ap)) (PlusOrMinus (b, bp)) = PlusOrMinus (ExactFraction.add a b, ExactFraction.add ap bp)

subtract :: Fraction -> Fraction -> Fraction
subtract a b = Fraction.add a (flipSign b)

multiply :: Fraction -> Fraction -> Fraction
multiply (Exact a) (Exact b) = Exact (ExactFraction.multiply a b)
multiply (PlusOrMinus (a, ap)) (Exact b) = PlusOrMinus (ExactFraction.multiply a b, ap)
multiply (Exact a) (PlusOrMinus (b, bp)) = PlusOrMinus (ExactFraction.multiply a b, bp)
multiply (PlusOrMinus (a, ap)) (PlusOrMinus (b, bp)) = PlusOrMinus (ExactFraction.multiply a b, plusOrMinus)
    where
        plusOrMinus = ExactFraction.add (ExactFraction.multiply a bp) (ExactFraction.multiply b ap)

divide :: Fraction -> Fraction -> Fraction
divide a b = Fraction.multiply a (reciprocal b)