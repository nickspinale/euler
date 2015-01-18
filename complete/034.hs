main = print $ sum [ n | n <- [10..999999], n == sum [ factorials !! read [d] | d <- show n ] ]

factorials = scanl (*) 1 [1..]
