-- (BNF) expr ::= term(+expr | )

data Expr = Val Int | Add Expr Expr deriving Show

expr' :: Parser Expr
expr' = do t <- term
           do symbol "+"
              e <- expr'
              return (Add (Val t) e)
            <|> return (Val t)

