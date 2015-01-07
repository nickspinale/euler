-- main = print . (+ length "one thousand") . sum . map letters $ [1..999]

letters :: Int -> Int
letters n = two + three + if two * three == 0 then 0 else 3
  where
    digs = reverse (show n) ++ repeat '0'
    two = f (take 2 digs)
    three = case digs !! 2
            of   '0' -> 0
                 h   -> g h + 7

-- ones place
g '0' = 0
g '1' = 3
g '2' = 3
g '3' = 5
g '4' = 4
g '5' = 4
g '6' = 3
g '7' = 5
g '8' = 5
g '9' = 4

-- tens place
h '1' = 4
h '2' = 6
h '3' = 6
h '4' = 5
h '5' = 5
h  d  = g d + 2

-- first two places (reversed)
f "01" = 3
f "11" = 8
f "21" = 6
f "31" = 8
f "51" = 7
f [o, '0'] = g o
f [o,  t ] = g o + h t
