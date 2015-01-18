import Data.List

main = print $ find ( (> 500)
                    . product
                    . map ((+ 1) . length)
                    . group
                    . reduce primes
                    )
                    $ map (sum . enumFromTo 1) [1..]

reduce :: [Int] -> Int -> [Int]
reduce _ 1 = []
reduce x@(p:ps) n = case mod n p
                    of   0 -> p : reduce x (div n p)
                         _ -> reduce ps n

primes :: [Int]
primes = sieve [2..]

sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]
