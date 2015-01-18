import Data.List

main = print $ sum $ nub [ (read $ drop (a' + b') p :: Int)
                         | p <- permutations "123456789"
                         , a' <- [1..7]
                         , b' <- [ 1 .. 8 - a' ]
                         , let (a, rest) = splitAt a' p
                               (b, c)    = splitAt b' rest
                           in read a * read b == (read c :: Int)
                         ]
