nodes :: Tree a -> Int
nodes (Node _ ts) = 1 + sum (map nodes ts)

depth :: Tree a -> Int
depth (Node _ []) = 0
depth (Node _ ts) = 1 + maximum (map depth ts)

