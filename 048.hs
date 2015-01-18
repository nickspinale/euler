main = print $ sum [ n ^ n `mod` 10 ^ 10 | n <- [1..1000] ]
