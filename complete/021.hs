import Data.List
import Control.Monad

main = print . sum $ shrink [ (n, d n) | n <- [1..9999] ]

shrink [] = []
shrink ((y, z):xs) = shrink xs ++ if (z, y) `elem` xs then [y, z] else []

swap (x, y) = (y, x)

d :: Int -> Int
d = sum . init . foldl (liftM2 (*)) [1] . map (scanl (*) 1) . group . reduce [2..]

reduce :: [Int] -> Int -> [Int]
reduce _ 1 = []
reduce x@(y:ys) n = case mod n y
                    of   0 -> y : reduce x (div n y)
                         _ ->     reduce ys     n
