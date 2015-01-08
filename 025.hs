import Data.List

-- returns index in list, so add one
main = print $ findIndex (>= (10 ^ 999)) fib

fib@(_:xs) = 1 : 1 : zipWith (+) fib xs
