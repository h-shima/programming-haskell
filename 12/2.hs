instance Functor ((->) a) where
  -- fmap ((->) r b) -> ((->) a r) -> ((->) a b)
  -- 関数のfmapは関数合成になる
  fmap = (.)


