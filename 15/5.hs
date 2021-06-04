data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat :: Tree a -> Tree a
repeat Leaf = Leaf
repeat (Node l x r) = Node (repeat (Node l x r)) x (repeat (Node l x r))

take :: Int -> Tree a -> Tree a
-- 本当は、0を渡した場合は失敗してほしいが、型を直すのが面倒なのでLeafを返すことにした
take 0 _ = Leaf 
take n Leaf         = Leaf
take n (Node l x r) = Node (take (n-1) l) x (take (n-1) r)

replicate :: Int -> Tree a -> Tree a
replicate n = take n . repeat

