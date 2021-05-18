data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap g (Node l v r) = foldMap g l `mappend` g v `mappend` foldMap g r

