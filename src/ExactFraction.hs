module ExactFraction (
    ExactFraction,
    ExactFraction.add,
    ExactFraction.subtract,
    ExactFraction.multiply,
    ExactFraction.divide,
) where

type ExactFraction = (Integer, Integer)

simplify a = simplify' $ normalise a
add a b = simplify $ add' (normalise a) (normalise b)
subtract a b = simplify $ subtract' (normalise a) (normalise b)
multiply a b = simplify $ multiply' (normalise a) (normalise b)
divide a b = simplify $ divide' (normalise a) (normalise b)

normalise :: ExactFraction -> ExactFraction
normalise (n, d)
    | n == 0 = (0, 1)
    | d < 0 = (-n, -d)
    | otherwise = (n, d)

simplify' :: ExactFraction -> ExactFraction
simplify' (n, d) = (simpleN, simpleD)
    where
        commonDen = gcd n d
        simpleN = n `div` commonDen
        simpleD = d `div` commonDen

add' :: ExactFraction -> ExactFraction -> ExactFraction
add' (an, ad) (bn, bd)
    | ad == bd = (an + bn, ad)
    | otherwise = (an * bd + bn * ad, ad * bd)

subtract' :: ExactFraction -> ExactFraction -> ExactFraction
subtract' (an, ad) (bn, bd) = add' (an, ad) (-bn, bd)

multiply' :: ExactFraction -> ExactFraction -> ExactFraction
multiply' (an, ad) (bn, bd) = (an * bn, ad * bd)
        
divide' :: ExactFraction -> ExactFraction -> ExactFraction
divide' (an, ad) (bn, bd) = multiply' (an, ad) (bd, bn)
