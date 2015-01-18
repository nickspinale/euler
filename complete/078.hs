import Data.List

main = print $ elemIndex 0 [ p n n | n <- [0..] ]

-- x = max coins that can be taken
-- y = coins left

p :: Int -> Int -> Int
p _ 0 = 1
p x y = sum [ (mem !! t) !! (y - t) | t <- [ 1 .. min x y ] ] `mod` 1000000

mem = [ [ p x y | y <- [0..] ] | x <- [0..] ]
