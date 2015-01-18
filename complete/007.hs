main = print $ sieve [2..] !! 10000

sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]
