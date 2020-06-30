module Type (
    Operand(..),
    Operator(..),
    Expression(..),
    Accuracy(..),
    Context(..),
    Fraction,
    defaultContext,
    approximate,
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
    Tangent |

    Approximate

    deriving (Eq, Show)

type SimpleFraction = (Integer, Integer)

data Accuracy =
    Perfect |
    PlusOrMinus SimpleFraction
    deriving (Eq, Show)

type Fraction = (Integer, Integer, Accuracy)
type Variable = Char

data Expression =
    Fraction Fraction |
    Variable Variable |
    Expression ([Expression], Operator)
    deriving (Eq, Show)

type Operand = Expression

data Context = Context {
    accuracy :: Accuracy,
    maxDecimalCalculations :: Integer,
    maxDisplayedDecimals :: Integer
} deriving (Eq, Show)

defaultContext :: Context
defaultContext = Context {accuracy = Perfect, maxDecimalCalculations = 10, maxDisplayedDecimals = 5}

approximate :: Context -> Bool
approximate Context {accuracy = Perfect} = False
approximate _ = True
