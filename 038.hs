import Data.List

main = print . find p . concatMap (map (map read) . init . groupings) $ perms "987654321"

p :: [Int] -> Bool
p (n:ns) = and (zipWith (==) ns $ tail (scanl1 (+) $ repeat n))

groupings :: [a] -> [[[a]]]
groupings [] = return []
groupings xs = do
    (a, b) <- splits [] xs
    c <- groupings b
    return (a : c)

splits :: [a] -> [a] -> [([a], [a])]
splits as [] = []
splits as (b:bs) = (c, bs) : splits c bs
  where c = as ++ [b]

perms :: Eq a => [a] -> [[a]]
perms [] = return []
perms xs = do
    y <- xs
    ys <- perms $ delete y xs
    return (y:ys)
