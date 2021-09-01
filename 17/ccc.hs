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

-- Target language:
  data Code = HALT | PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code

-- Compiler:
  comp :: Expr → Code
  comp x = comp’ x HALT
  comp’ :: Expr → Code → Code
  comp’ (Val n) c = PUSH n c
  comp’ (Add x y) c = comp’ x (comp’ y (ADD c))
  comp’ Throw c = FAIL
  comp’ (Catch x h) c = MARK (comp’ h c) (comp’ x (UNMARK c))

-- Virtual machine:
  type Stack = [Elem]
  data Elem = VAL Int | HAN Code
  exec :: Code → Stack → Stack
  exec HALT s = s
  exec (PUSH n c) s = exec c (VAL n :s)
  exec (ADD c) (VAL m:VAL n :s) = exec c (VAL (n+m):s)
  exec FAIL s = fail s
  exec (MARK c’ c) s = exec c (HAN c’ :s)
  exec (UNMARK c) (VAL n :HAN :s) = exec c (VAL n :s)
  fail :: Stack → Stack
  fail [ ] = [ ]
  fail (VAL n :s) = fail s
  fail (HAN c :s) = exec c s

