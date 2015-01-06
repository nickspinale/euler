import Control.Monad

main = print . msum $ do
    x <- [    1 .. 1000 ]
    y <- [x + 1 .. div (1000 - x) 2 ]
    let z = 1000 - x - y
    return $ if   x^2 + y^2 == z^2 && x + y + z == 1000
             then Just $ x * y * z
             else Nothing
