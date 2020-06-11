module InteractiveSolve (
    solveInteractive
) where

import Data.Maybe

import Definitions
import Solve

solveInteractive :: IO ()
solveInteractive = solveWithFacts []

solveWithFacts :: [Fact] -> IO ()
solveWithFacts facts = do
    input <- readLn
    if (input /= "quit") then do
        let (result, fact) = solve input facts
        putStrLn result
        if (isJust fact)
            -- TODO: figure out why my type assertion of `isJust` is not working
            then solveWithFacts (("x", 0) : facts)
            else solveWithFacts facts
    else do
        putStrLn "Exiting!"

