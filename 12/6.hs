instance Monad (a ->) where
  return x = \_ -> x

  h >>= f = \w -> f (h w) w

