import Test.Hspec
import Control.Exception (evaluate)
import ParseSpec
import SolveSpec
import OperandSpec
import DecimalSpec
import ExactFractionSpec

main :: IO ()
main = do
    parseSpec
    operandSpec
    decimalSpec
    solveSpec
    exactFractionSpec
