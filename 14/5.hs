filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = filter f . foldMap toList

