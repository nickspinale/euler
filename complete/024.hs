import Control.Monad

main = print (perms [0..9] !! 999999)

others _ [] = []
others xs (y:zs) = (y, xs ++ zs) : others (xs ++ [y]) zs

perms [] = [[]]
perms list = do
    (x, yz) <- others [] list
    zy <- perms yz
    return (x : zy)

-- perms :: [a] -> [a] -> [[a]]
-- perms [] [] = [[]]
-- perms xs [y] = liftM (y:) $ perms [] xs
-- perms xs (y:zs) = (liftM (y:) $ perms xs zs) ++ perms (y:xs) zs
