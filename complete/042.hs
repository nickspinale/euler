main = readFile "042.txt" >>= ( print
                              . length
                              . filter (isTriangular . sum . map charVal)
                              . (read :: String -> [String])
                              . ('[':)
                              . (++ "]")
                              )

charVal char = fromEnum char - fromEnum 'A' + 1

isTriangular n = aux 0
  where
    m = 2 * n
    aux curr = if prod < m then aux next else prod == m
      where
        next = curr + 1
        prod = curr * next
