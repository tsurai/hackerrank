--Contributed by Ron Watkins
module Main where

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib n = fibs!!(n-1)

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ inpu
