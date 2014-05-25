import Data.List

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- (n über k) = n! / ((n-k)! * k!)
-- (i-1)!/((j-1)!*(i-j)!
calc :: Int -> Int -> Int
calc i j = (factorial (i)) `div` (factorial (j) * factorial (i-j))

calcLine :: Int -> String
calcLine i = intercalate " " $ map (\j -> show $ (calc i j)) [0..i]

main = do
    i <- readLn :: IO Int
    putStrLn $ unlines $ map calcLine [0..(i-1)]
