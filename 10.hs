main = print . sum $ sieve [2..2000000]

sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]
