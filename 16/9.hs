-- data Maybe a = Nothing | Just a

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- アプリカティブ則
pure id <*> x = x -- (1)
pure (g x) = pure g <*> pure x -- (2)
x <*> pure y = pure (\g -> g y) <*> x -- (3)
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> -- (4)

(1)について
Nothingのとき:
pure id <*> Nothing
= (Just id) <*> Nothing -- pureを適用
= Nothing -- <*>を適用

Just xのとき:
pure id <*> (Just x)
= (Just id) <*> (Just x) -- pureを適用
= Just (id x) -- <*>を適用
= Just x -- idを適用


(2)について
Nothingのとき:
pure (g Nothing)
= Just (g Nothing) -- pureを適用
= fmap g (Just Nothing) -- fmapを逆適用
= pure g <*> Just Nothing -- <*>を逆適用
= pure g <*> pure Nothing -- pureを逆適用

Just aのとき:
pure (g (Just a))
= Just (g (Just a)) -- pureを適用
= fmap g (Just (Just a)) -- fmapを逆適用
= pure g <*> Just (Just a) -- <*>を逆適用
= pure g <*> pure (Just a) -- pureを逆適用

(3)について
Nothingのとき:
Nothing <*> pure y
= Nothing -- <*>を適用(自信なかったので、実際にNothingになることをghciで確認した)
= fmap (\g -> g y) Nothing -- fmapを逆適用
= pure (\g -> g y) <*> Nothing -- <*>を逆適用

Just aのとき:
Just a <*> pure y
= fmap a (pure y) -- fmapを逆適用
= fmap a (Just y) -- pureを適用
= Just (a y) -- fmapを適用
= Just ((\g -> g y) a) -- (\g -> g y)を逆適用(\g -> g y にaを適用すると、a y になるため)
= fmap (\g -> g y) (Just a) -- fmapを逆適用
= pure (\g -> g y) <*> Just a -- <*>を逆適用

(4)について
???
場合わけの仕方からしてわからん

