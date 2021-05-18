instance Monoid a => Monoid (Maybe a) where
  -- mempty :: a
  mempty = Nothing

  -- mappend ::
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance Foldable Maybe where
  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse g (Just x) = fmap Just (g x)

