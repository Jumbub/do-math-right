module Type (
    Operand(..),
    Operator(..),
    Expression(..),
) where

data Operator =

    Addition |
    Subtraction |
    Multiplication |
    Division |
    Power |

    LeftParentheses |
    RightParentheses |

    Decimal |
    Negation |

    Sine |
    Cosine |
    Tangent

    deriving (Eq, Show)

type Fraction = (Int, Int)
type Variable = Char

data Expression a b = Fraction a | Variable b | Expression ([Expression a b], Operator)
    deriving (Eq, Show)

type Operand = Expression (Int, Int) Char
