main = print $ last $ reduce 600851475143 [2..]

reduce 1 _ = []
reduce n x@(p:ps) = case mod n p
                    of   0 -> p : reduce (div n p) x
                         _ -> reduce n ps
