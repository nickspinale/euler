{-# LANGUAGE ParallelListComp #-}

-- Still pretty slow, but at least dynamic and using eulers generator function

import Data.List

main = print $ elemIndex 0 ps

ps :: [Int]
ps = 1 : map p [1..]

p :: Int -> Int
p n = sum
    [ sign (ps !! (n - pentagonal))
    | pentagonal <- takeWhile (<= n) pentagonals
    | sign <- cycle [id, id, negate, negate]
    ] `mod` 1000000

-- (generalized pentagonals)
pentagonals :: [Int]
pentagonals =
    [ sign n * (3 * sign n - 1) `div` 2
    | n <- [1..] >>= replicate 2
    | sign <- cycle [id, negate]
    ]

-- VERY SLOW APPROACH (naive)

-- main = print $ elemIndex 0 [ p n n | n <- [0..] ]

-- -- x = max coins that can be taken
-- -- y = coins left

-- p :: Int -> Int -> Int
-- p _ 0 = 1
-- p x y = sum [ (mem !! t) !! (y - t) | t <- [ 1 .. min x y ] ] `mod` 1000000

-- mem = [ [ p x y | y <- [0..] ] | x <- [0..] ]
