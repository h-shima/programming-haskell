instance Monad (a ->) where
  return x = \x -> x

  h >>= f = \w -> f (h w) w

