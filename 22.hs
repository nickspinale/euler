import Data.List

main = readFile "names.txt" >>= ( print
                                . sum
                                . zipWith (*) [1..]
                                . map (sum . map (\l -> fromEnum l - fromEnum 'A' + 1))
                                . sort
                                . (read :: String -> [String])
                                . ('[' : )
                                . (++ "]")
                                )
