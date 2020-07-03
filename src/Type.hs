module Type (
    Operand(..),
    Operator(..),
    Expression(..),
    Accuracy(..),
    isExactAccuracy,
    fromPlusOrMinus,
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

isExactAccuracy :: Accuracy -> Bool
isExactAccuracy Exact = True
isExactAccuracy (PlusOrMinus _) = False

fromPlusOrMinus :: Accuracy -> SimpleFraction
fromPlusOrMinus (PlusOrMinus fraction) = fraction
fromPlusOrMinus _ = error "Nu-uh."

data Accuracy =
    Exact |
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
defaultContext = Context {accuracy = Exact, maxDecimalCalculations = 10, maxDisplayedDecimals = 5}

approximate :: Context -> Bool
approximate Context {accuracy = Exact} = False
approximate _ = True
