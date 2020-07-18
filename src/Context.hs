module Context (
    Context(..),
    defaultContext,
) where

data Context = Context {
    decimalResult :: Bool,
    decimalPlaces :: Integer
} deriving (Eq, Show)

defaultContext :: Context
defaultContext = Context {decimalResult = True, decimalPlaces = 5}
