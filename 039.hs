groupings n = 

l = map (`lazyElem` squares) [0..]

squares = map (^2) [0..]

lazyElem x xs = aux xs where aux (y:ys) = if x > y then aux ys else x == y
