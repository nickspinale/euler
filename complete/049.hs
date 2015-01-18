import Data.Array
import Data.List

main = print
    [  (n, x, y)
    |  n <- primes
    ,  (x:ys) <- tails
               . map (read :: String -> Int)
               . tail
               . permutations
               $ show n
    ,  y <- ys
    ,  x > 999
    && y > 999
    && x /= n
    && y /= n
    && y /= x
    && prime ! x
    && prime ! y
    && y - x == x - n
    ]

-- Memoize isprimes
prime = listArray (1000, 9999) $ map (`elem` primes) [1000..9999]

primes = filter (> 999) . takeWhile (< 10000) $ sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
