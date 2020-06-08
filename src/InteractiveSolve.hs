module InteractiveSolve
    ( solve
    ) where

solve :: IO ()
solve = do
    input <- readLn
    putStrLn input
