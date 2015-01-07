main = getContents >>= (print . take 10 . read . sum . map (read :: String -> Integer) . lines)

-- TURNS OUT THEY MEANT THE FIRST 10 DIGITS FROM THE OTHER SIDE

-- main = getContents >>= ( print
--                        . concatMap show
--                        . reverse
--                        . take 10
--                        . carry
--                        . reverse
--                        . sums
--                        . map (map $ read . (:[]))
--                        . lines
--                        )

-- sums ([]:_) = []
-- sums x = sum (map head x) : sums (map tail x)

-- carry (x:y:zs) = mod x 10 : carry (div x 10 + y : zs)
