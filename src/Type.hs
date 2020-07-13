module Type (
    Operand(..),
    Operator(..),
    Expression(..),
) where

import Fraction

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
    Tangent |

    Approximate |
    PlusOrMinusOperator

    deriving (Eq, Show)

type Variable = Char

data Expression =
    Fraction Fraction |
    Variable Variable |
    Expression ([Expression], Operator)
    deriving (Eq, Show)

type Operand = Expression
