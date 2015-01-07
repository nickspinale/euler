import           Data.List
import           Control.Monad.State

import qualified Data.Map as M

main = print
     . maximumBy (compare `on` snd)
     . M.assocs
     . execState (sequence_ $ map chain [1..1000000])
     $ M.fromList [(1, 0)]

-- memoized
chain :: Int -> State (M.Map Int Int) Int
chain n = do
    result <- gets $ M.lookup n
    case result of
        Just len -> return len
        Nothing  -> do
            nextResult <- chain $ if even n then div n 2 else 3 * n + 1
            let len = 1 + nextResult
            modify $ M.insert n len
            return len
