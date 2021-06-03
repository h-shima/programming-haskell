data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- repeat :: a -> [a]
-- repeat x = xs where xs = x:xs
-- ここで止まってる、木をrepeatするっていう概念がわからない
repeat :: Tree a -> [Tree a]

repeat Leaf = xs where xs = Leaf:xs
repeat (Node l x r) = xs where xs = (Node l x r):xs

