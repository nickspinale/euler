main = print . length $ concatMap say [1..999] ++ "onethousand"

say :: Int -> String
say n = putAnd (hundreds h) (two [o, t])
  where (o:t:h:_) = reverse (show n) ++ repeat '0'

putAnd "" b = b
putAnd a "" = a
putAnd a  b = a ++ "and" ++ b

two "00" = ""
two [o,'0'] = ones o
two "01" = "ten"
two "11" = "eleven"
two "21" = "twelve"
two "31" = "thirteen"
two "41" = "fourteen"
two "51" = "fifteen"
two "61" = "sixteen"
two "71" = "seventeen"
two "81" = "eighteen"
two "91" = "nineteen"
two [o, t ] = tens t ++ ones o

-- ones place
ones '0' = ""
ones '1' = "one"
ones '2' = "two"
ones '3' = "three"
ones '4' = "four"
ones '5' = "five"
ones '6' = "six"
ones '7' = "seven"
ones '8' = "eight"
ones '9' = "nine"

tens '2' = "twenty"
tens '3' = "thirty"
tens '4' = "forty"
tens '5' = "fifty"
tens '6' = "sixty"
tens '7' = "seventy"
tens '8' = "eighty"
tens '9' = "ninety"

hundreds '0' = ""
hundreds  h  = ones h ++ "hundred"
