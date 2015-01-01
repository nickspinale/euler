{-# LANGUAGE TupleSections #-}

import Data.List
import Control.Monad.State.Lazy

primes = seive [2..]

-- seive of erasthos
seive (n:ns) = n : seive [ n' | n' <- ns, mod n' n > 0 ]

-- order of each (anonymous) prime factorizatio of n!
reduceF :: Int -> [Int]
reduceF n = groupify $ concatMap reduce $ [2..n]

-- prime factorization
reduce :: Int -> [Int]
reduce n = aux primes
  where
    aux :: [Int] -> [Int]
    aux (x:xs) | n == x = [n]
               | mod n x == 0 = x : reduce (div n x)
               | otherwise = aux xs

-- count instances of (anonymous) elements in a list
groupify :: Eq a => [a] -> [Int]
groupify [] = []
groupify (x:xs) = count x xs : groupify (filter (/= x) xs)

-- note extra 1 in first equation: this is because of its use above
count _ [] = 1
count n (x:xs) = (if n == x then 1 else 0) + count n xs

--------------------------------------

choose :: Int -> [a] -> [[a]]
choose n xs = choose' (length xs) n xs

choose' :: Int -> Int -> [a] -> [[a]]
choose' _ 0 _ = return []
choose' len n (x:xs) =
    liftM (x :) (choose' (len - 1) (n - 1) xs)
    ++ if len <= n
       then []
       else choose' (len - 1) n xs

-- W . toSnd (!)
wfact :: Int -> Int -> Int
wfact fact n = length $ filter check $ choose n factors
  where
    reduced :: [Int]
    reduced = reduceF fact
    factors :: [[Int]]
    factors = mapM (enumFromTo 0) reduced
    check :: [[Int]] -> Bool
    check = all (== 0) . foldl (zipWith (-)) reduced

main = print $ wfact 100 10

--------------------------------------
-- WIERD StateT _ [] IDEAS
--------------------------------------
-- lazy length check
--lazylen :: Int -> [a] -> Bool
--lazylen 0 [] = True
--lazylen _ [] = False
--lazylen n (x:xs) = lazylen (n - 1) xs

--singles :: [a] -> [(a, [a])]
--singles = singles' []

--singles' _ [] = []
--singles' past (present:future) = (present, past ++ future) : singles' (present:past) future

----allfactors :: [Int] -> [[Int]]
--allfactors = singles . mapM (enumFromTo 0)

--go 1 current _ = if all (== 0) current then return [current] else []
--go togo current bench = do
--    (this, others) <- bench
--    let new = zipWith (-) current this
--    if any (< 0) new
--     then []
--     else do
--        rest <- go (togo - 1) new $ singles others
--        return (this : rest)

--t = groupify $ reduce 144
--test = go 4 t (allfactors t)
--------------------------------------

--subj :: ([Int], [[Int]])
--subj = ([3, 2, 4], [])

----strip :: StateT ([Int], [[Int]]) [] [Int]
----strip used left = case 
----    [ (current, (zipWith (-) left current, current : used))
----    | current <- mapM (enumFromTo 0) left
----    , not $ current `elem` used
----    ]

--strip :: StateT ([Int], [[Int]]) [] [Int]
--strip = StateT $ \(left, used) ->
--    [ (current, (zipWith (-) left current, current : used))
--    | current <- mapM (enumFromTo 0) left
--    , not $ current `elem` used
--    ]

--go :: StateT ([Int], [[Int]]) [] [[Int]]
--go = do
--    factor <- strip
--    (left, used) <- get
--    if all (== 0) left && elem left used
--      then return [factor]
--      else do
--        others <- go
--        return (factor : others)

--test = filter (lazylen 4) $ evalStateT go off

--off = (groupify $ reduce 144, [])
--------------------------------------

--nexts :: StateT (Int, [Int]) [] Int
--nexts = StateT $ \(curr, illegals) -> [ (new, (curr - new, new : illegals))
--                                      | new <- [0..curr]
--                                      , not $ elem new illegals
--                                      ]

            --prepare :: [Int] -> [] (Int, [Int])
            --prepare = map (, [])

--nexts :: StateT (Int, [Int]) [] Int
--nexts = StateT $ \(curr, illegals) ->
--    [ (new, (curr - new, new : illegals))
--    | new <- [0..curr]
--    , not $ elem new illegals
--    ]

      --nexts :: (Int, [Int]) -> [(Int, (Int, [Int]))]
      --nexts (curr, illegals) =
      --    [ (new, (curr - new, new : illegals))
      --    | new <- [0..curr]
      --    , not $ elem new illegals
      --    ]

      --take1 :: StateT [(Int, [Int])] [] [Int]
      --take1 = StateT $ map unzip . mapM nexts

      --takeAll :: StateT [(Int, [Int])] [] [[Int]]
      --takeAll = do
      --    st <- get
      --    if all (null . nexts) st
      --      then return []
      --      else do one <- take1
      --              rest <- takeAll
      --              return (one : rest)

      --test = evalStateT takeAll $ prepare [2, 4, 3]

--take1 :: [(Int, [Int])] -> [] [(Int, [Int])]
--take1 [] = return []
--take1 (x:xs) = do
--    first <- nexts x
--    rest <- take1 xs
--    return (first : rest)

--combos :: [] (Int, [Int]) -> [[[(Int, [Int])]]]
--combos [] = return []
--combos xs = do
--    one <- take1 xs
--    others <- case combos one of
--                [] -> return []
--                c -> c
--    return (one : others)

--test = map length . combos . prepare . groupify . reduce