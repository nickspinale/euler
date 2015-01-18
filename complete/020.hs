main = print . sum . map ((read :: String -> Int) . (:[])) . show $ factorial 100

factorial 1 = 1
factorial n = n * factorial (n - 1)
