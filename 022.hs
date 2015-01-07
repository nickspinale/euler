import Data.List

main = readFile "022.txt" >>= ( print
                              . sum
                              . zipWith (*) [1..]
                              . map (sum . map (\l -> fromEnum l - fromEnum 'A' + 1))
                              . sort
                              . (read :: String -> [String])
                              . ('[' : )
                              . (++ "]")
                              )
