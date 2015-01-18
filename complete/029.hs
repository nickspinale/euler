import Data.List
import Control.Monad

main = print $ length $ nub $ liftM2 (^) [2..100] [2..100]

-- A FAILED ATTEMPT TO FIND A BETTER SOLUTION

-- main = print $ length $ filter ok $ liftM2 (,) [2..limit] [2..limit]

-- ok (a, b) = aux 2
--   where
--     aux n | a ^ n > limit = True
--           | b `mod` n == 0 && b /= n = False
--           | otherwise = aux (n + 1)

                      -- , and [ b `mod` n > 0
                            -- | n <- let aux n' a' = if a' > limit
                                                   -- then []
                                                   -- else n' : aux (n' + 1) (a' * a)
                                   -- in aux 2 (a * a)
                            -- ]
                                  -- $ filter ((> 0) . mod b) $ 
                      -- , and [ b `mod` n > 0
                            -- | (n, _) <- zip [2..] $ takeWhile (<= limit)
                                                  -- $ tail
                                                  -- $ scanl1 (*)
                                                  -- $ repeat a

-- shave :: [(Int, Int)] -> Int
-- shave [] = 0
-- shave (x:xs) = 1 + shave (deletes (altForms x) xs)

-- altForms x@(a, b) = aux x
--   where
--     aux (a', b') =
--         case mod b' 2
--         of   0 -> let x' = (a' * a, div b' 2)
--                   in  x' : aux x'
--              _ -> []
--   -- where
--   --   altForms (_, 1) = []
--   --   altForms x'@(a', b') = x' : altForms (a' * a, b' - 1)

-- powersOf = scanl (*) 1 . repeat

-- deletes [] ys = ys
-- deletes _ [] = []
-- deletes z@(x:xs) (y:ys) = if x == y then deletes xs ys else y : deletes z ys

-- shave [] = 0
-- shave ((a, b):xs) = 1 + shave (
-- shave [] = 0
-- shave ((a, b):xs) = 1 + shave [ x | x@(a', b') <- xs, maybe True (> div b b') $ powerOf a' a ]

-- powerOf y x = aux y
--   where
--     aux 1  = return 0
--     aux y' = case mod y' x
--              of   0 -> liftM (+1) $ aux $ div y' x
--                   _ -> Nothing
