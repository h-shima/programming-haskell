instance Applicative ((->) a) where
  -- pure :: r -> ((->) a) r
  pure x = (\_ -> x)

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) f g = \x -> f x (g x)

