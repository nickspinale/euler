{-# LANGUAGE ParallelListComp #-}

import Data.List

main = print $ sum [ (read n :: Integer)
                   | n <- permutations "0123456789"
                   , and [ mod x y == 0
                         | x <- tail $ threes n
                         | y <- [2,3,5,7,11,13,17]
                         ]
                   ]

threes :: String -> [Integer]
threes (a:bs@(b:c:ds)) = read [a,b,c] : threes bs
threes _ = []
