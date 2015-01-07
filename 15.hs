main = print $ g 20 20

-- number of possible interspersals, memoized
interspersals :: [[Int]]
interspersals = zipWith map (map f [0..]) $ repeat [0..]

g x y = (interspersals !! x ) !! y

f 0 _ = 1
f _ 0 = 1
f x y = g (x - 1) y + g x (y - 1)
