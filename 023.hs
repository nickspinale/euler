import Control.Monad
import Data.List
import Data.Array

limit :: Int
limit = 28123

main = print $ sum
    [ n
    | n <- [1..limit]
    , not $ or [ abounds ! (n - a1) | a1 <- takeWhile (< n) abundants ]
    ]

abundants = filter (abounds !) [1..limit]

-- Memoized abounds
abounds = listArray (1, limit)
    [ n < sum ( init
              . foldl (liftM2 (*)) [1]
              . map (scanl (*) 1)
              . group
              $ reduce primes n
              )
    | n <- [1..limit]
    ]

reduce _ 1 = []
reduce y@(x:xs) n = case n `mod` x of
    0 -> x : reduce y (div n x)
    _ -> reduce xs n

-- Faster than reducing from all numbers
primes = sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

-- Slow abounds
-- abounds n = n < aux (n - 1)
--   where
--     aux 0 = 0
--     aux d = case n `mod` d of
--               0 -> aux (d - 1) + d
--               _ -> aux (d - 1)

-- lazyDiff [] _ = []
-- lazyDiff a [] = a
-- lazyDiff a@(x:xs) b@(y:ys)
--     | x > y     =     lazyDiff a  ys
--     | x < y     = x : lazyDiff xs b
--     | otherwise =     lazyDiff xs ys
