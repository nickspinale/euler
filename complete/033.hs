-- UGLY
-- TODO: make less ugly

import Data.List

main = print . simplify . clump . unzip . nub $
    [ simplify (a, b)
    | a <- [1..8]
    , b <- [a + 1..9]
    , c <- [1..9]
    , any (simplify (a, b) ==) $ map simplify
       [ (10 * c + a, c + 10 * b)
       , (10 * c + a, 10 * c + b)
       , (c + a * 10, c + 10 * b)
       , (c + a * 10, 10 * c + b)
       ]
    ]

clump :: ([Int], [Int]) -> (Int, Int)
clump (x, y) = (product x, product y)

simplify :: (Int, Int) -> (Int, Int)
simplify (a, b) = ( product (a' \\ i), product (b' \\ i))
  where
    a' = reduce a
    b' = reduce b
    i = intersect a' b'

reduce x = aux primes x
  where
    aux _ 1 = []
    aux y@(p:ps) z = case z `mod` p of
        0 -> p : aux y (z `div` p)
        _ -> aux ps z

primes = sieve [2..]

sieve (x:xs) = x : sieve [ y | y <- xs, y `mod` x > 0 ]
