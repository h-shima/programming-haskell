-- a. 自然数と左結合の減算演算子　構文規則
-- expr ::= expr - nat | nat
-- nat ::= 0 | 1 | 2 | ...

-- b. 上記構文規則をパーサに変換
expr :: Parser Int
expr = do e <- expr
          symbol "-"
          n <- natural
          return (e - n)
        <|> natural

-- c. 上記パーサの問題点
左再帰となり、無限ループが発生する（それを避けるために、書籍では単純な演算子にも右結合を採用している）

-- d. manyとfoldlを使って書き直す
-- 右再帰にすれば良いはずだが、、分からず 
-- expr :: Parser Int
-- expr = do xs <- many

