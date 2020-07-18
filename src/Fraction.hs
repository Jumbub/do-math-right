module Fraction (
    Fraction,
    Fraction.fromExact,
    Fraction.simplify,
    Fraction.flipSign,
    Fraction.reciprocal,
    Fraction.add,
    Fraction.subtract,
    Fraction.multiply,
    Fraction.divide,
    Fraction.sin,
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
        plusOrMinus = ExactFraction.divide dist (fromNumber 2)
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
        plusOrMinus = ExactFraction.divide dist (fromNumber 2)
        output = ExactFraction.add minN plusOrMinus

multiply :: Fraction -> Fraction -> Fraction
multiply a b = operateOnFractionAB ExactFraction.multiply a b

divide :: Fraction -> Fraction -> Fraction
divide a b = operateOnFractionAB ExactFraction.divide a b

powerN :: Integer -> Fraction -> Fraction
powerN n a = operateOnFractionA (ExactFraction.powerN n) a

biggestExact :: Fraction -> ExactFraction
biggestExact (a, ap) = ExactFraction.add a ap

sin :: Integer -> Fraction -> Fraction
sin dp x = (result, (1, 10 ^ dp))
    where
        (result, _) = sin' dp x x True x x 1 1
        sin' :: Integer -> Fraction -> Fraction -> Bool -> Fraction -> Fraction -> Integer -> Integer -> Fraction
        sin' iters acc@(xn, xp) lastVal lastAdd lastNum x lastDen lastFact
            | iters == 0 = (xn, ExactFraction.add xp (biggestExact lastVal))
            | otherwise = sin' (iters - 1) acc' currentVal (not lastAdd) num x den (lastFact + 2)
            where
                acc' = operator acc currentVal
                operator = if (lastAdd) then Fraction.subtract else Fraction.add
                currentVal = Fraction.divide num ((den, 1), (0, 1))
                num = Fraction.multiply x $ Fraction.multiply x lastNum
                den = lastDen * (lastFact + 1) * (lastFact + 2)
