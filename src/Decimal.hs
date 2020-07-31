module Decimal (
    Decimal(..),
    Sign(..),
    decimalToString,
    fractionToDecimal,
) where

import Data.List
import Data.Map
import Data.Maybe
import Debug.Trace

import OperandType
import Utility
import Fraction
import ExactFraction
import Context

data Sign =
    Positive |
    Negative
    deriving (Show, Eq)

data Decimal =
    ExactDecimal (Sign, Integer, [Integer]) |
    RecurringDecimal (Sign, Integer, [Integer], [Integer]) |
    PlusOrMinusDecimal (Sign, Integer, [Integer], (Integer, Integer))
    deriving (Show, Eq)

fractionToDecimal :: Context -> Fraction -> Decimal
fractionToDecimal ctx frac = normalise $ fractionToDecimal' ctx frac

fractionToDecimal' :: Context -> Fraction -> Decimal
fractionToDecimal' _ ((_, 0), _) = error "Cannot divide by 0!"
fractionToDecimal' context ((numerator, denominator), accuracy@(accNum, accDen))
    | isMoreInnaccurate = PlusOrMinusDecimal (sign, whole, decimalsWithoutRemsLimited, innaccuracy')
    | isInnaccurate = PlusOrMinusDecimal (sign, whole, decimalsWithoutRemsLimited, (accNum, accDen))
    | isRecurringDecimal = RecurringDecimal (sign, whole, beforeRecurring, theRecurring)
    | isJust plusOrMinus = fromJust plusOrMinus
    | otherwise = ExactDecimal (sign, whole, decimalsWithoutRemsLimited)
    where
        isMoreInnaccurate = isInnaccurate && (genericLength decimalsWithoutRems > precision)
        isInnaccurate = accNum /= 0
        (accNum, accDen) = accuracy
        decAccuracy = (1, (10 ^ precision))
        innaccuracy = ExactFraction.add (accNum, accDen) decAccuracy
        innaccuracy' = if ExactFraction.compare accuracy decAccuracy == LT then decAccuracy else innaccuracy
        sign = if numerator < 0 then Negative else Positive
        numerator' = abs numerator
        whole = numerator' `div` denominator
        remainder = numerator' `rem` denominator
        precision = Context.decimalPlaces context
        hiddenPrecision = precision + 5
        plusOrMinus = findPlusOrMinus sign whole precision decimalsWithoutRems
        recurringPattern = findRecurring Data.Map.empty decimalsWithRems []
        isRecurringDecimal = isJust recurringPattern && ((genericLength beforeRecurring + genericLength theRecurring) <= precision)
        (beforeRecurring, theRecurring) = fromMaybe ([], []) recurringPattern
        decimalsWithRems = findDecimalsWithRems hiddenPrecision denominator remainder []
        decimalsWithoutRems = Data.List.map (\(dec, rem) -> dec) decimalsWithRems
        decimalsWithoutRemsLimited = Decimal.removeTrailingZeros $ Data.List.take (fromIntegral precision) decimalsWithoutRems

normalise :: Decimal -> Decimal
normalise (ExactDecimal (Negative, 0, [])) = ExactDecimal (Positive, 0, [])
normalise (RecurringDecimal (Negative, 0, [], [])) = RecurringDecimal (Positive, 0, [], [])
normalise (PlusOrMinusDecimal (Negative, 0, [], p)) = PlusOrMinusDecimal (Positive, 0, [], p)
normalise x = x

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

findPlusOrMinus :: Sign -> Integer -> Integer -> [Integer] -> Maybe Decimal
findPlusOrMinus sign whole precision decimals
    | isPlusOrMinus = Just $ PlusOrMinusDecimal (sign, whole, decimals'', (1, 10 ^ precision))
    | otherwise = Nothing
    where
        isPlusOrMinus = genericLength decimals > precision
        decimals' = Data.List.take (fromIntegral precision) decimals
        decimals'' = Decimal.removeTrailingZeros $ decimals'

digitsToString :: [Integer] -> String
digitsToString digits = concat $ Data.List.map show digits

decimalToString :: Decimal -> String
decimalToString (ExactDecimal (sign, whole, [])) = addSign sign $ show whole
decimalToString (ExactDecimal (sign, whole, decimals)) = addSign sign $ show whole ++ "." ++ digitsToString decimals
decimalToString (RecurringDecimal (sign, whole, decimals, [])) = decimalToString (ExactDecimal (sign, whole, decimals))
decimalToString (RecurringDecimal (sign, whole, [], recurring))
    = decimalToString (ExactDecimal (sign, whole, [])) ++ ".(" ++ digitsToString recurring ++ ")"
decimalToString (RecurringDecimal (sign, whole, decimals, recurring))
    = decimalToString (ExactDecimal (sign, whole, decimals)) ++ "(" ++ digitsToString recurring ++ ")"
decimalToString (PlusOrMinusDecimal (sign, whole, decimals, (numerator, 1)))
    = decimalToString (ExactDecimal (sign, whole, decimals)) ++ " ± " ++ show numerator
decimalToString (PlusOrMinusDecimal (sign, whole, decimals, (numerator, denominator)))
    = decimalToString (ExactDecimal (sign, whole, decimals)) ++ " ± " ++ show numerator ++ "/" ++ show denominator

addSign :: Sign -> String -> String
addSign Positive decimal = decimal
addSign Negative decimal = ('-':decimal)

removeTrailingZeros :: [Integer] -> [Integer]
removeTrailingZeros x = reverse $ removeTrailingZeros' $ reverse x
    where
        removeTrailingZeros' :: [Integer] -> [Integer]
        removeTrailingZeros' [] = []
        removeTrailingZeros' (0:xs) = removeTrailingZeros' xs
        removeTrailingZeros' x = x
