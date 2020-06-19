import Test.Hspec
import Control.Exception (evaluate)
import ParseSpec
import SolveSpec
import OperandSpec

main :: IO ()
main = do
    parseSpec
    operandSpec
    solveSpec
