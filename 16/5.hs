基底部:
  take 0 xs ++ drop 0 xs
= [] ++ xs -- 定義より
= xs -- ++を適用

基底部:
  take n [] ++ drop n []
= [] ++ [] -- 定義より
= [] -- ++を適用

再帰部:
  take (n+1) (x:xs) ++ drop (n+1) (x:xs)
= (x : take n xs) ++ drop (n+1) (x:xs) -- takenを適用
= (x : take n xs) ++ drop n xs -- dropの適用
= x : ((take n xs) ++ (drop n xs)) -- ++の定義より
= x : xs   -- 仮定より

