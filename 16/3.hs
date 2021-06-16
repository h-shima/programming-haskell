基底部:
  all (== x) (replicate 0 x)
= all (== x) [] -- replicateの定義
= True -- allの定義

再帰部:
  all (== x) (replicate (n+1) x)
= all (== x) (x : replicate n x)
= (== x) x && all (== x) (replicate n x) -- allを適用
= True && all (== x) (replicate n x) -- (== x) xを簡約
= all (== x) (replicate n x) -- &&を適用
= True -- 仮定より(任意のnの時、与式は成り立つ)

以上.

