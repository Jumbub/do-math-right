import Test.Hspec
import Control.Exception (evaluate)
import ParseSpec
import SolveSpec
import OperandSpec
import DecimalSpec

main :: IO ()
main = do
    parseSpec
    operandSpec
    decimalSpec
    solveSpec
