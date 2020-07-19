module FractionType (
    Fraction,
) where

import ExactFraction

type Fraction = (ExactFraction, ExactFraction)
