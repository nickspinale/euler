main = readFile "008.txt" >>= ( print
                              . maximum
                              . findAdjs
                              . map ((read :: String -> Int) . (:[]))
                              . concat
                              . lines
                              )

findAdjs [] = []
findAdjs x  = multNext 13 x : findAdjs (tail x)

multNext _ [] = 1
multNext 0 _  = 1
multNext n (x:xs) = x * multNext (n - 1) xs
