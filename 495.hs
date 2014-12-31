import Data.List

main = return ()

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

-- is list no larger than n
limited :: Int -> [a] -> Bool
limited 0 _ = True
limited n (x:xs) = limited (n - 1) xs
limited _ _ = False

prepare :: [Int] -> [[Int]]
prepare = map $ enumFromTo 0

singles :: [Int] -> [(Int, [Int])]
singles = singles' []

singles' :: [a] -> [a] -> [(a, [a])]
singles' _ [] = []
singles' past (present:future) = (present, past ++ future) : singles' (present:past) future

f :: [[Int]] -> [(Int, [Int])]
f [] = []
f (x:xs) = do
    parted <- singles x
    rest <- f xs
    return (parted : rest)

-- given a list of counts, gives 
-- combos :: [Int] -> [[[Int]]]
combos xs = do
    (useds, lefts) <- f xs
    rest <- case combos lefts
            of   [] -> return []
                 c  -> c
    return (useds : rest)

-- split = map unzip . f . prepare
test = map unzip . combos . prepare $ [2, 2, 2]




-- split :: [Int] -> [([Int], [Int])]
-- split = map unzip . tail . split'

-- split' :: [Int] -> [[(Int, Int)]]
-- split' [] = [[]]
-- split' (x:xs) = do
--     used <- [0..x]
--     rest <- split' xs
--     return ((used, x - used) : rest)
