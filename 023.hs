import Data.List
import Control.Monad

limit :: Integer
limit = 28123

main = print . sum $ foldr filter [1..limit] [ \x -> abounds (x - first) | first <- abundants ]

abundants = filter abounds [1..limit]

abounds n = n < sum ( init
                    . foldl (liftM2 (*)) [1]
                    . map (scanl (*) 1)
                    . group
                    $ reduce primes n
                    )

-- lazyDiff [] _ = []
-- lazyDiff a [] = a
-- lazyDiff a@(x:xs) b@(y:ys)
--     | x > y     =     lazyDiff a  ys
--     | x < y     = x : lazyDiff xs b
--     | otherwise =     lazyDiff xs ys

reduce :: [Integer] -> Integer -> [Integer]
reduce _ 1 = []
reduce y@(x:xs) n = case n `mod` x of
    0 -> x : reduce y (div n x)
    _ -> reduce xs n

-- Faster than reducing from all numbers
primes = sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

-- abounds n = n < aux (n - 1)
--   where
--     aux 0 = 0
--     aux d = case n `mod` d of
--               0 -> aux (d - 1) + d
--               _ -> aux (d - 1)
