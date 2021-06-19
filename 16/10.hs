-- モナド則
return x >>= f = f x                          ... (1)
mx >>= return  = mx                           ... (2)
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g)) ... (3)

-- リストモナド
instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]

(1)
return x >>= f
= [x] >>= f -- returnを適用
= f x -- >>=を適用

(2)
基底部:
[] >>= return
= [y | x <- [], y <- return x] -- 定義より
= [] -- リスト内包表記を簡約

再帰部:
(z:zs) >>= return
= [y | x <- (z:zs), y <- return x] -- 定義より
= [y | x <- (z:zs), y <- [x]] -- returnを適用
= [y | x <- [z] ++ zs, y <- [x]] -- ++を逆適用
= [y | x <- [z], y <- [x]] ++ [y | x <- zs, y <- [x]] -- 分割
= [z] ++ zs -- リスト内包表記を簡約
= z:zs -- ++を適用

(3)
基底部:
([] >>= f) >>= g
= [y | x <- [], y <- f x] -- >>=を適用
= [] >>= g -- リスト内包表記を簡約
= [y | x <- [], y <- f x] -- >>=を適用
= [] -- リスト内包表記を簡約
= [y | z <- [], y <- (\x -> (f x >>= g)) z] -- 求めたい答えから逆算...（スマートなやり方がわからなかった）
= [] >>= (\x -> (f x >>= g))

再帰部:
((x:xs) >>= f) >>= g
= [y | z <- (x:xs), y <- f z] >>= g -- >>=を適用
= [b | a <- [y | z <- (x:xs), y <- f z], b <- g a] -- >>=を適用
= [b | a <- [y | z <- [x], y <- f z], b <- g a] ++ [b | a <- [y | z <- xs, y <- f z], b <- g a] -- ++を逆適用
= [b | a <- f x, b <- g a]                      ++ [b | a <- [y | z <- xs, y <- f z], b <- g a] -- リスト内包表記を簡約
= [b | a <- f x, b <- g a]                      ++ [y | z <- xs, y <- f z] >>= g -- >>=を逆適用
= [b | a <- f x, b <- g a]                      ++ [y | z <- xs] >>= f >>= g -- >>=を逆適用
= [b | a <- f x, b <- g a]                      ++ (xs >>= f >>= g) -- リスト内包表記を簡約
= [b | a <- f x, b <- g a]                      ++ (xs >>= (\w -> (f w >>= g))) -- 仮定より
= [b | a <- f x, b <- g a]                      ++ [y | z <- xs, y <- (\w -> (f w >>= g)) z] -- >>=を適用
= [b | a <- f x, b <- g a]                      ++ [y | z <- xs, y <- (f z >>= g)] -- 無名関数を簡約
= [b | a <- f x, b <- g a]                      ++ (xs >>= (f z >>= g)) -- >>=を逆適用
= [b | a <- f x, b <- g a]                      ++ (xs >>= [b | a <- f z, b <- g a]) -- >>=を適用
= [b | a <- f x, b <- g a]                      ++ [y | z <- xs, y<- [b | a <- f z, b <- g a]] -- >>=を適用
= [y | y <- [b | a <- f x, b <- g a]]           ++ [y | z <- xs, y<- [b | a <- f z, b <- g a]] -- リスト内包表記を変形
= [y | z <- [x], y <- [b | a <- f z, b <- g a]] ++ [y | z <- xs, y<- [b | a <- f z, b <- g a]] -- a <- f xを逆適用
= [y | z <- (x:xs), y <- [b | a <- f z, b <- g a]]
= [y | z <- (x:xs), y <- (\w -> [b | a <- f w, b <- g a]) z] -- \wを逆適用
= (x:xs) >>= (\w -> [b | a <- f w, b <- g a]) -- >>=を逆適用 
= (x:xs) >>= (\w -> (f w >>= g))  -- >>=を逆適用

