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

    PiOperand |

    Approximate |
    PlusOrMinusOperator |
    SetDp

    deriving (Eq, Show)
