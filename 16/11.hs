comp' e c = comp e ++ c

基底部:
comp' (Val n) c
= comp (Val n) ++ c -- 定義より
= [PUSH n] ++ c -- compを適用
= PUSH n : c

再帰部:
comp' (Add x y) c
= comp (Add x y) ++ c -- 定義より
= comp x ++ comp y ++ [ADD] ++ c -- compを適用
= comp x ++ (comp y ++ ([ADD] ++ c)) -- 結合則より
= comp x ++ (comp y ++ ([ADD] : c)) -- ++を適用
= comp x ++ (comp y ++ (ADD : c))
= comp x ++ (comp' y (ADD : c)) -- 仮定より
= comp' x (comp' y (ADD : c)) -- 仮定より

