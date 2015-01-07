main = print . sum . filter even $ takeWhile (<= 4000000) fib

fib@(x:xs) = 1 : 1 : zipWith (+) fib xs
