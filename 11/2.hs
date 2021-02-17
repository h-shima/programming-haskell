-- 関数play'のみ変更する
play' :: Grid -> Player -> IO ()
play' g p
  | wins o g = putStrLn "Player o wins!\n"
  | wins x g = putStrLn "Player x wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == o   = do i <- getNat (prompt p)
                 case move g i p of
                   [] -> do putStrLn "ERROR: Invalid move"
                            play' g p
                   [g'] -> play g' (next p)
  | p == x   = do putStr "Player x is thinking... "
                  let gs = bestmoves g p
                  n <- randomRIO (o, length gs - 1) -- ここだけ
                  play (gs !! n) (next p)

