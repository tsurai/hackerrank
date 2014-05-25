main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    putStrLn $ show $ length $ filter (== x) $ map sum $ foldr (\u v -> let z = v ++ map (u:) v in filter (\w -> x >= sum w) z) [[]] $ reverse $ takeWhile (<= x) $ map (^y) $ [1..x]
