-- 文法
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

-- 意味
eval :: Expr -> Maybe Int
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

-- ターゲット言語の種類
data Code = HALT
          | PUSH Int Code
          | ADD Code
          | MARK Code
          | UNMARK Code
          | FAIL
          deriving (Show)

-- コンパイラ(目標言語であるCodeまで)
comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw c = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

-- Stack
type Stack = [Int]

-- Virtual Machine
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (MARK h c) s = exec c (h : s)

-- WIP
-- MARK, UNMARKの扱い
-- exec（Code -> Stack）の定義

-- https://pdfs.semanticscholar.org/8ec2/9246b708ca149a4dc05d088a3b4f7396b14b.pdf

