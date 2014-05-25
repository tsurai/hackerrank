type Queen = (Int, Int)

collision :: Queen -> Queen -> Bool
collision (x1,y1) (x2,y2) =
    x1 == x2 || -- same row
    y1 == y2 || -- same col
    dx == dy || -- diagonal
    l           -- L shape
    where dx = abs (x1 - x2)
          dy = abs (y1 - y2)
          l  = (dx == 1 && dy == 2) || (dx == 2 && dy == 1)

search :: Int -> Int -> [Queen] -> Int
search x n q
    | x == (n+1) = 1
    | otherwise =
        let set = filter (\y -> not $ any (collision (x,y)) q) [1..n]
        in if set == [] then
            0
        else
            sum $ map (\q' -> search (x+1) n ((x,q'):q)) set

main = do
    n <- readLn :: IO Int
    putStrLn $ show $ (search 1 n [])
