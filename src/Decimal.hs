module Decimal (
    Decimal(..),
    decimalToString,
    fractionToDecimal,
) where

import Data.List
import Data.Map
import Data.Maybe
import Debug.Trace

import Type

data Decimal =
    PerfectDecimal (Integer, [Integer]) |
    RecurringDecimal (Integer, [Integer], [Integer]) |
    PlusOrMinusDecimal (Integer, [Integer], (Integer, Integer))
    deriving (Show, Eq)

fractionToDecimal :: Context -> Fraction -> Decimal
fractionToDecimal _ (_, 0, _) = error "Cannot divide by 0!"
fractionToDecimal context (numerator, denominator, Perfect)
    | isJust recurringPattern = RecurringDecimal (whole, beforeRecurring, theRecurring)
    | otherwise = PerfectDecimal (whole, decimalsWithoutRems)
    where
        whole = numerator `div` denominator
        remainder = numerator `rem` denominator
        precision = maxDecimalCalculations context
        hiddenPrecision = maxDisplayedDecimals context
        decimalsWithRems = findDecimalsWithRems hiddenPrecision denominator remainder []
        recurringPattern = findRecurring Data.Map.empty decimalsWithRems []
        (beforeRecurring, theRecurring) = fromJust recurringPattern
        decimalsWithoutRems = Data.List.map (\(dec, rem) -> dec) decimalsWithRems

findDecimalsWithRems :: Integer -> Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
findDecimalsWithRems a b c d | trace (show (a, b, c, d)) False = undefined
findDecimalsWithRems precision divisor lastRemainder decimals
    | maxPrecision || dividingZero = decimals
    | otherwise = findDecimalsWithRems precision divisor remainder (decimals ++ [(decimal, lastRemainder)])
    where
        maxPrecision = genericLength decimals >= precision
        dividingZero = lastRemainder == 0
        isRecursive = False
        current = lastRemainder * 10
        decimal = current `div` divisor
        remainder = current `rem` divisor

type RecurringMap = Map (Integer, Integer) ([Integer], [Integer])

findRecurring :: RecurringMap -> [(Integer, Integer)] -> [Integer] -> Maybe ([Integer], [Integer])
findRecurring a b c | trace (show ("findRecurring", (a, b, c))) False = undefined
findRecurring recs decs prev
    | Data.List.null decs = Nothing
    | noPattern = findRecurring recs'' (tail decs) (prev ++ [decimalValue])
    | otherwise = Just (fromJust pattern)
    where
        noPattern = isNothing pattern
        dec = head decs
        pattern = Data.Map.lookup dec recs
        (decimalValue, _) = dec
        recs' = Data.Map.insert dec (prev, []) recs
        recs'' = Data.Map.map (\(prev, rec) -> (prev, rec ++ [decimalValue])) recs'

-- addToStates :: Integer -> Integer -> [DivState] -> [DivState]
-- addToStates cur rem states =
--     where
--         withNewState = DivState { stateCur = cur, stateRem = rem, statePre = }
--         withUpdatedStates = map (addToState cur) withNewState

-- addToState :: Integer -> DivState -> DivState
-- addToState digit state = state { stateRec = (stateRec state ++ [digit]) }

-- fractionToDecimal _ _ 0 = error "Cannot divide by 0!"
-- fractionToDecimal acc num den = (div num den, (fractionToDecimal' acc den (rem num den) []))

-- fractionToDecimal' :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
-- fractionToDecimal' precision divisor remainder decimals
--     | maxPrecision = []
--     | noRemainder = decimals
--     | otherwise = fractionToDecimal' precision divisor (rem numberToDivide divisor) (decimals ++ [div numberToDivide divisor])
--     where
--         noRemainder = remainder == 0
--         maxPrecision = length decimals == precision
--         numberToDivide = 10 * remainder

digitsToString :: [Integer] -> String
digitsToString digits = concat $ Data.List.map show digits

decimalToString :: Decimal -> String
decimalToString (PerfectDecimal (whole, [])) = show whole
decimalToString (PerfectDecimal (whole, decimals)) = show whole ++ "." ++ digitsToString decimals
decimalToString (RecurringDecimal (whole, decimals, [])) = decimalToString (PerfectDecimal (whole, decimals))
decimalToString (RecurringDecimal (whole, [], recurring))
    = decimalToString (PerfectDecimal (whole, [])) ++ ".(" ++ digitsToString recurring ++ ")"
decimalToString (RecurringDecimal (whole, decimals, recurring))
    = decimalToString (PerfectDecimal (whole, decimals)) ++ "(" ++ digitsToString recurring ++ ")"
decimalToString (PlusOrMinusDecimal (whole, decimals, (numerator, denominator)))
    = decimalToString (PerfectDecimal (whole, decimals)) ++ " ± " ++ show numerator ++ "/" ++ show denominator
