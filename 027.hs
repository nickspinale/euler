import Data.List

main = print
      $ maximumBy
        (\x y -> compare (consecs x) $ consecs y)
        [ (a, b) | a <- [-999..999], b <- takeWhile (< 1000) primes ]

consecs (a, b) = length $ takeWhile isPrime [ n * n + a * n + b | n <- [0..] ]

isPrime x = if x < 2 then False else aux x primes
  where aux n (p:ps) = if n > p then aux n ps else n == p

primes = sieve [2..]

sieve (x:xs) = x : sieve (filter ((> 0) . (`mod` x)) xs)
