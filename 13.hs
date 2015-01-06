main = getContents >>= ( print
                       . concatMap show
                       . take 10
                       . carry
                       . reverse
                       . sums
                       . map (map $ read . (:[]))
                       . lines
                       )

sums ([]:_) = []
sums x = sum (map head x) : sums (map tail x)

carry (x:y:zs) = mod x 10 : carry (div x 10 + y : zs)
