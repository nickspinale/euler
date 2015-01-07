main = readFile "067.txt" >>= ( print
                              . best
                              . map (map (read :: String -> Int) . words)
                              . lines
                              )

-- dynamic
best (x:[]) = x
best (x:xs) = zipWith (+) x $ zipWith max z ys
  where z@(_:ys) = best xs
