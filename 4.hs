import Data.List
import Control.Monad

main = print $ maximum [ n | n <- liftM2 (*) [100..999] [100..999], show n == reverse (show n) ]
