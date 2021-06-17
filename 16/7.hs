data Maybe = Nothing | Just a deriving Show

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

-- 関手則
fmap id = id ... (i)
fmap (g . h) = fmap g . fmap h ... (ii)


-- (i)
-- Nothingの時
fmap id Nothing
= Nothing -- 定義より
= id Nothing -- idの定義より

-- Justの時
fmap id (Just x)
= Just (id x)
= Just x -- 定義より
= id (Just x) -- idの定義より

-- (ii)
-- Nothingの時
fmap (g . h) Nothing
= Nothing -- 定義より
= fmap h Nothing -- fmapを逆適用
= fmap g (fmap h Nothing) --fmapを逆適用
= (fmap g . fmap h) Nothing -- .を逆適用

-- Justの時
fmap (g . h) Just x
= Just ((g . h) x)
= Just (g (h x)) -- .を適用
= fmap g (Just (h x)) -- 定義より
= fmap g (fmap h (Just x)) -- fmapを逆適用
= (fmap g . fmap h) (Just x) -- .を逆適用

