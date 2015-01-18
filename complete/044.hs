import Data.Maybe
import Control.Monad
import Data.List

main = print $ msum
    [ if pentagon (p1 + p2) && pentagon (p1 + p2 + p2)
      then Just p1
      else Nothing
    | (n, p1) <- [ (n, pentify n) | n <- [1..] ]
    , p2 <- [ pentify m | m <- [n .. (p1 - 1) `div` 3] ]
    , pentagon p2
    ]

pentify :: Integer -> Integer
pentify n = n * (3 * n - 1) `div` 2

root :: Integer -> Maybe Integer
root n = search 1 n
  where
    search low high
        | low > high = Nothing
        | midd < n   = search (mid + 1) high
        | midd > n   = search low (mid - 1)
        | otherwise  = Just mid
      where
        mid = (high + low) `div` 2
        midd = mid * mid

pentagon :: Integer -> Bool
pentagon n = isJust $ do
    r <- root (24 * n + 1)
    if r `mod` 6 == 5
     then Just ()
     else Nothing

-- check :: [(Integer, Integer)] -> Maybe Integer
-- check ((p1, _):r1) = aux r1
--   where
--     aux ((p2, d2):r2) =
--         if d2 > p1
--         then Nothing
--         else these `mplus` aux r2
--       where
--         these = do
--             let p3 = p1 + p2
--             r3 <- lazyIn p3 r2
--             let p4 = p2 + p3
--             lazyIn p4 r3
--             return p1

-- -- squares = map (^2) [1..]

-- lazyIn :: Integer -> [(Integer, Integer)] -> Maybe [(Integer, Integer)]
-- lazyIn x xs = aux xs
--   where
--     aux ((y,_):ys)
--         | x > y = aux ys
--         | x < y = Nothing
--         | otherwise = Just ys
