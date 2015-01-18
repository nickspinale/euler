import Data.List

main = print . find goldbachwaswrong $ lazyDiff [3, 5 ..] primes

goldbachwaswrong n = not $ or [ square $ (n - p) `div` 2 | p <- takeWhile (< n) primes ]

square n = search 1 n
  where
    search low high
        | low > high = False
        | midd < n   = search (mid + 1) high
        | midd > n   = search low (mid - 1)
        | otherwise  = True
      where
        mid = (high + low) `div` 2
        midd = mid * mid

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

lazyDiff [] _ = []
lazyDiff a [] = a
lazyDiff a@(x:xs) b@(y:ys)
    | x > y     =     lazyDiff a  ys
    | x < y     = x : lazyDiff xs b
    | otherwise =     lazyDiff xs ys
