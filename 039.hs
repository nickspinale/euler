import Data.List

main = print $ maximumBy (\(_, s1) (_, s2) -> compare s1 s2) [ (p, solutions p) | p <- [1..1000] ]
solutions n = length [ () | a <- [1..n], b <- [a..n], a ^ 2 + b ^ 2 == (n - a - b) ^ 2 ]

    -- a <- [ 1 .. n `div` 3 ] -- rounded from 2 + sqrt 2
    -- b <- [ a .. 

-- l = map (`lazyElem` squares) [0..]

-- squares = map (^2) [0..]

-- lazyElem x xs = aux xs where aux (y:ys) = if x > y then aux ys else x == y
