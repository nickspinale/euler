import Numeric
import Data.Char

main = print $ sum [ x
                   | x <- [0..1000000]
                   , palandrome (show x)
                  && palandrome (showIntAtBase 2 intToDigit x "")
                   ]

palandrome x = x == reverse x
