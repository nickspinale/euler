main = print $ last $ reduce 600851475143 $ sieve [2..]

sieve (x:xs) = x : [ y | y <- xs, mod y x /= 0 ]

reduce 1 _ = []
reduce n x@(p:ps) = case mod n p
                    of   0 -> p : reduce (div n p) x
                         _ -> reduce n ps
