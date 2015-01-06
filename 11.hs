import Control.Monad

main = do
    input <- getContents
    let grid = map (map (read :: String -> Int) . words) $ lines input
        f x y = (grid!!y)!!x
    print . maximum $  [ product $ liftM (     f x) [y..y+3] | x <- [0..19], y <- [0..16] ]
                    ++ [ product $ liftM (flip f y) [x..x+3] | x <- [0..16], y <- [0..19] ]
                    ++ [ f x y * f (x+1) (y+1) * f (x+2) (y+2) * f (x+3) (y+3) | x <- [0..16], y <- [0..16] ]
                    ++ [ f x y * f (x-1) (y+1) * f (x-2) (y+2) * f (x-3) (y+3) | x <- [3..19], y <- [0..16] ]
