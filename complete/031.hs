main = print $ divide [200, 100, 50, 20, 10, 5, 2] 200

divide [] _ = 1
divide (x:xs) n = sum [ divide xs (n - x * y) | y <- [ 0 .. n `div` x ] ]
