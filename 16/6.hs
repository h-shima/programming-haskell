-- 木
data Tree' = Leaf Int | Node Tree' Tree' deriving Show

-- 葉の数を数える関数
countLeaf :: Tree' -> Int
countLeaf (Leaf _) = 1
countLeaf (Node lhs rhs) = countLeaf lhs + countLeaf rhs

-- 節の数を数える関数
countNode :: Tree' -> Int
countNode (Leaf _) = 0
countNode (Node lhs rhs) = countNode lhs + 1 + countNode rhs

基底部:
countNode (Leaf 1)
= 0 -- 定義より

= countLeaf (Leaf 1)
= 1 -- 定義より

よって
countLeaf (Leaf 1) = countNode (Leaf 1) + 1
が成り立つ.

再帰部:
countNode (Node l r) + 1
= countNode l + 1 + countNode r + 1 -- countNodeを適用
= countLeaf l + countNode r + 1 -- 仮定より
= countLeaf l + countLeaf r -- 仮定より
= countLeaf (Node l r) -- countLeafを逆適用

