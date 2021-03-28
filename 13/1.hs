-- haskellコメントのパーサ
comment = do string "--"
             many (sat (/= '\n'))
             return ()

