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
import Utility

data Decimal =
    ExactDecimal (Integer, [Integer]) |
    RecurringDecimal (Integer, [Integer], [Integer]) |
    PlusOrMinusDecimal (Integer, [Integer], (Integer, Integer))
    deriving (Show, Eq)

fractionToDecimal :: Context -> Fraction -> Decimal
fractionToDecimal _ (_, 0, _) = error "Cannot divide by 0!"
fractionToDecimal context (numerator, denominator, accuracy)
    | isMoreInnaccurate = PlusOrMinusDecimal (whole, decimalsWithoutRemsLimited ++ [5], innaccuracy)
    | isInnaccurate = PlusOrMinusDecimal (whole, decimalsWithoutRemsLimited, (accNum, accDen))
    | isRecurringDecimal = RecurringDecimal (whole, beforeRecurring, theRecurring)
    | isJust plusOrMinus = fromJust plusOrMinus
    | otherwise = ExactDecimal (whole, decimalsWithoutRemsLimited)
    where
        isMoreInnaccurate = isInnaccurate && (genericLength decimalsWithoutRems > precision)
        isInnaccurate = not $ isExactAccuracy accuracy
        (accNum, accDen) = fromPlusOrMinus accuracy
        innaccuracy = simplifyFraction $ Utility.addFraction (accNum, accDen) (5, 10 ^ (precision + 1))
        whole = numerator `div` denominator
        remainder = numerator `rem` denominator
        precision = Type.decimalPlaces context
        hiddenPrecision = precision + 5
        plusOrMinus = findPlusOrMinus whole precision decimalsWithoutRems
        recurringPattern = findRecurring Data.Map.empty decimalsWithRems []
        isRecurringDecimal = isJust recurringPattern && ((genericLength beforeRecurring + genericLength theRecurring) <= precision)
        (beforeRecurring, theRecurring) = fromMaybe ([], []) recurringPattern
        decimalsWithRems = findDecimalsWithRems hiddenPrecision denominator remainder []
        decimalsWithoutRems = Data.List.map (\(dec, rem) -> dec) decimalsWithRems
        decimalsWithoutRemsLimited = Data.List.take (fromIntegral precision) decimalsWithoutRems

findDecimalsWithRems :: Integer -> Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
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

findPlusOrMinus :: Integer -> Integer -> [Integer] -> Maybe Decimal
findPlusOrMinus whole precision decimals
    | isPlusOrMinus = Just $ PlusOrMinusDecimal (whole, decimals', (1, 10 ^ precision))
    | otherwise = Nothing
    where
        isPlusOrMinus = genericLength decimals > precision
        decimals' = Data.List.take (fromIntegral precision) decimals

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
decimalToString (ExactDecimal (whole, [])) = show whole
decimalToString (ExactDecimal (whole, decimals)) = show whole ++ "." ++ digitsToString decimals
decimalToString (RecurringDecimal (whole, decimals, [])) = decimalToString (ExactDecimal (whole, decimals))
decimalToString (RecurringDecimal (whole, [], recurring))
    = decimalToString (ExactDecimal (whole, [])) ++ ".(" ++ digitsToString recurring ++ ")"
decimalToString (RecurringDecimal (whole, decimals, recurring))
    = decimalToString (ExactDecimal (whole, decimals)) ++ "(" ++ digitsToString recurring ++ ")"
decimalToString (PlusOrMinusDecimal (whole, decimals, (numerator, denominator)))
    = decimalToString (ExactDecimal (whole, decimals)) ++ " ± " ++ show numerator ++ "/" ++ show denominator
