main = print $ (foldl1 cross $ map (`map` [1..]) [t, p, h]) !! 2

t n = n * (    n + 1) `div` 2 
p n = n * (3 * n - 1) `div` 2
h n = n * (2 * n - 1)

-- Lazily intersects ordered lists
cross a@(x:xs) b@(y:ys)
    | x > y = cross a ys
    | x < y = cross xs b
    | otherwise = x : cross xs ys
