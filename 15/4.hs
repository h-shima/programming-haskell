fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs :: [Integer]
fibs = [x | x <- map fib [0..]]

