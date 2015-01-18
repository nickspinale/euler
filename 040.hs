import Data.Char

main = print
     . product
     . map (digitToInt . (concat (map show [1..]) !!) . pred)
     . take 7
     . scanl (*) 1
     $ repeat 10
