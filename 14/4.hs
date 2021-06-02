import Data.Foldable
import Data.Monoid

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f g)

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap g (Node l v r) = foldMap g l `mappend` g v `mappend` foldMap g r

  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r

