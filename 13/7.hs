-- expr ::= term (+ expr | - expr | )
-- term ::= exponent (* term | / term | )
-- exponent ::= factor^int | factor
-- factor ::= (expr) | int
-- int ::= ... | -1 | 0 | 1 | ...

expr :: Parser Int
expr = do
  t <- term
  do symbol "+"
     e <- expr
     return (t + e)
   <|> do symbol "-"
          e <- expr
          return (t - e)
   <|> return t

term :: Parser Int
term = do
  ex <- exponent
  do symbol "*"
     t <- term
     return (ex * t)
   <|> do symbol "/"
          t <- term
          return (ex `div` t)
   <|> return ex

exponent :: Parser Int
exponent = do
  f <- factor
  do symbol "^"
     i <- int
     return (f ^ i)
   <|> return f

