main = print . sum . takeWhile (< 2000000) $ sieve [2..]

sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]
