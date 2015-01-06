main = print . product $ sieve [1..20]

sieve [] = []
sieve (x:xs) = x : sieve [ case mod y x
                           of   0 -> div y x
                                _ -> y
                         | y <- xs
                         ]
