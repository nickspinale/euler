import Data.List

main = print $ maximumBy f [1..1000]

f x y = compare (digits x) (digits y)

digits x = aux [] (1, x)
  where
    aux past r@(n, d) = case elemIndex r past of
        Just dist -> dist
        _ -> aux (r:past) (10 * mod n d, d)

-- Silly state monad version

-- digits :: (Int, Int) -> State [(Int, Int)] Int
-- digits r@(n, d) = do
--     done <- gets $ elemIndex r
--     case done of
--         Just dist -> return dist
--         Nothing -> do
--             modify (r:)
--             digits (10 * mod n d, d)
