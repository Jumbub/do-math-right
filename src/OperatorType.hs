module OperatorType (
    Operator(..),
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

    PiOperand |

    Approximate |
    PlusOrMinusOperator

    deriving (Eq, Show)
