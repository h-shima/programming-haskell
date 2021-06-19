data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  -- fmap (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)= Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- 関手則
fmap id = id -- (1)
fmap (g . h) = fmap g . fmap h -- (2)

-- Tree型が関手則を満たすことを構造的帰納法で示す

(1)について
基底部:
fmap id (Leaf x)
= Leaf (id x) -- fmapを適用
= Leaf x -- idを適用
= id (Leaf x) -- idを逆適用

再帰部
fmap id (Node l r)
= Node (fmap id l) (fmap id r) -- fmapを適用
= Node l r -- 仮定より
= id (Node l r) -- idを逆適用


(2)について
基底部:
fmap (g . h) (Leaf x)
= Leaf ((g . h) x) -- fmapを適用
= Leaf (g (h x)) -- .を適用
= fmap g (Leaf (h x)) -- fmapを逆適用
= fmap g (fmap h (Leaf x)) -- fmapを逆適用
= fmap g . fmap h (Leaf x) -- .を逆適用

再帰部:
fmap (g.h) (Node l r)
= Node (fmap (g.h) l) (fmap (g.h) r) -- fmapを適用
= Node ((fmap g . fmap h) l) ((fmap g . fmap h) r) -- 仮定より
= Node (fmap g (fmap h l)) (fmap g (fmap h r)) -- .を適用
= fmap g (Node (fmap h l) (fmap h r)) -- fmapを逆適用
= fmap g (fmap h (Node l r)) -- fmapを逆適用
= fmap g . fmap h (Node l r) -- .を逆適用

