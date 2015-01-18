import Control.Monad
import Data.List

main = print
     . msum
     . map check
     $ tails [ ( n * (3 * n - 1) `div` 2
               , 3 * n + 1
               )
             | n <- [1..]
             ]

check ((p1, _):r1) = aux r1
  where
    aux ((p2, d2):r2) =
        if d2 > p1
        then Nothing
        else (lazyIn p3 r2 >>= lazyIn p4) `mplus` aux r2
      where
        p3 = p1 + p2
        p4 = p2 + p3

-- nexts :: Int -> [(Int, Int)] -> [(Int, [(Int, Int)])]
-- nexts p1 r = aux r
--   where
--     aux (e@(p2, d'):r')
--         | d' > p = []
--         | p3 `lazyIn` r' = (p2, r') : aux r'
--         | otherwise = aux r'
--       where p3 = p1 + p2

-- lazyIn :: Int -> [(Int, Int)] -> Maybe [(Int, Int)]
lazyIn x xs = aux xs
  where
    aux ((y,_):ys)
        | x < y = aux ys
        | x > y = Nothing
        | otherwise = Just ys
