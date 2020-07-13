module Context (
    Context(..),
    defaultContext,
    approximate,
) where

data Context = Context {
    fractionResult :: Bool,
    decimalPlaces :: Integer
} deriving (Eq, Show)

defaultContext :: Context
defaultContext = Context {fractionResult = True, decimalPlaces = 5}

approximate :: Context -> Bool
approximate Context {fractionResult = True} = False
approximate _ = True
