import Data.Foldable

data Maybe' a = Just' a | Nothing' deriving Show

instance Monoid a => Monoid (Maybe' a) where
  -- mempty :: a
  mempty = Nothing'

  -- mappend ::
  Nothing' `mappend` m = m
  m `mappend` Nothing' = m
  Just' m1 `mappend` Just' m2 = Just' (m1 `mappend` m2)

instance Foldable Maybe' where
  -- foldMap :: Monoid b => (a -> b) -> Maybe' a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x

  -- fold :: Monoid a => Maybe' a -> a
  fold Nothing' = mempty
  fold (Just' x) = x

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  -- foldl :: (a -> b -> a) -> a -> [b] -> a
  -- わからず...

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  (Just' f) <*> something = fmap f something

instance Traversable Maybe' where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing' = pure Nothing'
  traverse g (Just' x) = fmap Just' <*> g x
