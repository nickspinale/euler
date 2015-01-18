{-# LANGUAGE MultiWayIf #-}

main = print $ count 0 months

-- January 1st 1901 was a Tuesday, so "days % 7 == 5" on Sundays
count _ [] = 0
count days (m:ms) = this + count (days + m) ms
  where
    this = case days `mod` 7
           of   5 -> 1
                _ -> 0

months = [ if month == 2
           then ( if   year `mod` 100 /= 0
                     && year `mod`   4 == 0
                     || year `mod` 400 == 0
                   then 29
                   else 28
                )
            else ( if month `elem` [9, 4, 6, 11]
                   then 30
                   else 31
                 )
         | year <- [1901..2000]
         , month <- [1..12]
         ]
