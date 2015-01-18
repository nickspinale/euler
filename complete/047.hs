import Data.List
import Control.Monad

main = print
     . liftM head
     . find (all ((== 4) . length) . map (group . reduce primes))
     . map (take 4)
     $ tails ([1..] :: [Int])

reduce _ 1 = []
reduce y@(x:xs) n = case n `mod` x of
    0 -> x : reduce y (div n x)
    _ -> reduce xs n

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

-- Misunderstood problem at first:

-- distinct [] _ = True
-- distinct _ [] = True
-- distinct a@(x:xs) b@(y:ys)
--     | x > y = distinct a ys
--     | x < y = distinct xs b
--     | otherwise = False
