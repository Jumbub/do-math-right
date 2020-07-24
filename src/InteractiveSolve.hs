module InteractiveSolve (
    solveInteractive
) where

import Data.Maybe
import System.IO
import Control.Monad

import Solve
import Operand
import Operand
import Context

solveInteractive :: IO ()
solveInteractive = do
    putStrLn "\
        \╔═══════════════╗\n\
        \║ do-math-right ║\n\
        \╚═══════════════╝\n\
        \Enter '1+1'\n"
    solveInteractive' defaultContext

solveInteractive' :: Context -> IO ()
solveInteractive' context = do
    putStr ">  "
    hFlush stdout
    input <- getLine
    let (context', output) = solve context input
    putStrLn $ "=> " ++ output
    solveInteractive' context'
