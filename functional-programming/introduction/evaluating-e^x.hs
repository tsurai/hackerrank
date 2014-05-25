-- Enter your code here. Read input from STDIN. Print output to STDOUT
factorial :: Float -> Float  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

e :: Float -> Float
e x =
    let e2 x 10 = 0.0
        e2 x y = ((x ** y)/(factorial y)) + (e2 x (y+1))
    in 1 + x + (e2 x 2)

main = do
    n <- readLn :: IO Int
    input <- getContents
    mapM_ putStrLn $ map show $ map e $ map (read :: String -> Float) $ lines input
