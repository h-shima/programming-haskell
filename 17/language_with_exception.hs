-- 例外を扱える評価器
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

eval :: Expr -> Maybe int
eval (Val n)   = Just n
eval (Add x y) = case eval x of
                   Nothing -> Nothing
                   Just n  -> case eval y of
                                Nothing -> Nothing
                                Just m  -> Just (n+m)
eval Throw = Nothing
eval (Catch x h) = case eval x of
                     Nothing -> eval h
                     Just n  -> Just n

-- この言語のコンパイラの算出
-- この文献の最後の方に答えが載ってる。。。
-- https://pdfs.semanticscholar.org/8ec2/9246b708ca149a4dc05d088a3b4f7396b14b.pdf

