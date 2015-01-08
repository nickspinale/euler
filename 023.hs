import Data.List
import Control.Monad

limit = 28123

main = print . sum . (\\) [1..limit] . filter (<= limit) $ liftM2 (+) abundants abundants

abundants = [ n | n <- [1..limit], n < sum (factors n) ]

factors = init . foldl (liftM2 (*)) [1] . map (scanl (*) 1) . group . reduce [2..]

reduce :: [Int] -> Int -> [Int]
reduce _ 1 = []
reduce y@(x:xs) n = case mod n x of
    0 -> x : reduce y (div n x)
    _ -> reduce xs n
