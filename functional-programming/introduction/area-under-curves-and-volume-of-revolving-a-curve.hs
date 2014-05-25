import Text.Printf (printf)

-- compute value of polynomial with coefficients c, exponents e and input value x
f :: [Int] -> [Int] -> Double -> Double
f a b x = sum $ zipWith (\c d -> (fromIntegral c) * (x ^^ d)) a b

-- create all x values [l, l + 0.001, l + 0.002, ..., r]
values :: Int -> Int -> [Double]
values l r = takeWhile (<= fromIntegral r) $ dropWhile (<= fromIntegral l) $ map (*0.001) [0..]

-- compute area of rotation disk (c - coefficients, e - exponents, x - input value)
area :: [Int] -> [Int] -> Double -> Double
area a b x = let y = f a b x in (y * y) * pi

-- sum the given function f ("f" or "area") over the range [l, r] (coefficients and exponents are given in a and b)
summation :: Int -> Int -> [Int] -> [Int] -> ([Int] -> [Int] -> Double -> Double) -> Double
summation l r a b f = sum $  map (*0.001) $ map (f a b) (values l r)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [summation l r a b f, summation l r a b area]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
