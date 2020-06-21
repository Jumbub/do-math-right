module InteractiveSolve (
    solveInteractive
) where

import Data.Maybe
import System.IO

import Solve
import Operand

solveInteractive :: IO ()
solveInteractive = do
    putStrLn "\
        \╔═══════╗\n\
        \║do-math║\n\
        \╚═══════╝\n\
        \Enter '1+1'\n"
    solveInteractive'

solveInteractive' :: IO ()
solveInteractive' = do
    putStr ">  "
    hFlush stdout
    input <- getLine
    let output = solve input
    putStrLn $ "=> " ++ output
    solveInteractive'
