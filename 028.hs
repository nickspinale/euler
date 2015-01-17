main = print $ 1 + sum [ sum
                       $ take 4
                       $ iterate (+ (-2) * n)
                       $ (2 * n + 1) ^ 2
                       | n <- [1..500]
                       ]
