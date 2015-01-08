main = readFile "067.txt" >>= print . foldr1 f . map (map read . words) . lines

f :: [Int] -> [Int] -> [Int]
f x y = zipWith (+) x $ zipWith max y $ tail y
