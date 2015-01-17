main = print $ sum [ x | x <- [2..999999], x == sum [ read [d] ^ 5 | d <- show x ] ]
