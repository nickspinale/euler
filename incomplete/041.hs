import Control.Monad

main = print $ sortedIntersect (pandigitals [1..4] ++ pandigitals [1..7]) primes

-- p n = length difference + length showed == 10 && isPrefixOf difference digits
--   where
--     difference = digits \\ showed
--     showed = show n 

-- pandigital :: Integer -> Boolean
-- pandigital n = undefined
    
-- seperate e x = (e, y) : seperate (succ e) z
--   where (y, z) = span (< 10 ^ e) x

sortedIntersect :: Ord a => [a] -> [a] -> [a]
sortedIntersect [] _ = []
sortedIntersect _ [] = []
sortedIntersect xxs@(x:xs) yys@(y:ys) = case compare x y of
    EQ -> x : sortedIntersect xs ys
    LT -> sortedIntersect xs yys
    GT -> sortedIntersect xxs ys

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

pandigitals :: [Integer] -> [Integer]
pandigitals = fmap (foldl (\n d -> d + 10 * n) 0) . permutations

selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x, xs) : fmap (fmap (x:)) (selections xs)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
    (y, ys) <- selections xs
    zs <- permutations ys
    return (y:zs)

primes = sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
