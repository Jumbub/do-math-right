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

data Expression =
    Fraction Fraction |
    Variable Variable |
    Expression ([Expression], Operator)
    deriving (Eq, Show)

type Operand = Expression
