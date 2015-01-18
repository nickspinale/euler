import Data.List

main = print $ length [ ()
                      | n <- takeWhile (< 1000000) primes
                      , let digits = show n
                            order = length digits
                        in  and [ prime (read . take order . drop off $ cycle digits)
                                | off <- [ 1 .. order - 1 ]
                                ]
                      ]

-- factorials = scanl (*) 1 [1..]

prime n = aux primes
  where
    aux (x:xs)
        | x == n = True
        | x < n = aux xs
        | otherwise = False

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
