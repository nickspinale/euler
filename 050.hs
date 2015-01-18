-- Takes a while (18min)

import Data.Array
import Data.List

limit = (< 1000000)

main = print
     . maximumBy matchup
     . filter ((!) prime . snd)
     $ tails primes >>= takeWhile (limit . snd)
                      . map collapse
                      . inits

matchup (x, _) (y, _) = compare x y
collapse x = (length x, sum x)

-- Memoize isprimes (0 there for corner cases with inits and tails)
prime = listArray (0, 999999) $ map (`elem` primes) [0..999999]

primes = takeWhile limit $ sieve [2..]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
