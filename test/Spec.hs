import Test.Hspec
import Control.Exception (evaluate)
import ParseSpec
import SolveSpec

main :: IO ()
main = do
    parseSpec
    solveSpec
