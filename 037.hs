main = print . sum . take 11 . drop 4 $ intersect (left ps) (right 10 ps)
  where ps = seperate 10 primes

intersect (x:xs) (y:ys)
    | x < y = intersect xs (y:ys)
    | x > y = intersect (x:xs) ys
    | otherwise = x : intersect xs ys

left    (x:y:ys) = x ++ left           (filter ((`elem` x) . (`div` 10)) y : ys)
right e (x:y:ys) = x ++ right (e * 10) (filter ((`elem` x) . (`mod` e )) y : ys)

seperate e x = y : seperate (10 * e) z
  where (y, z) = span (< e) x

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
