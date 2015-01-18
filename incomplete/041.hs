import Data.List
import Data.Char

digits = ['9','8'..'0']
limit = 100000
-- limit = 1000000000

main = print . find p . reverse $ takeWhile (< limit) primes

p n = length difference + length showed == 10 && isPrefixOf difference digits
  where
    difference = digits \\ showed
    showed = show n 

-- seperate e x = (e, y) : seperate (succ e) z
--   where (y, z) = span (< 10 ^ e) x

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
