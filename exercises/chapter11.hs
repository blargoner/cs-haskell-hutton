-- exercise 4
fibs :: [Integer]
fibs = 0 : 1 : [ x + y | (x,y) <- zip fibs (tail fibs) ]

-- exercise 5
fib :: Int -> Integer
fib = (fibs !! )

fibk :: Integer
fibk = head (dropWhile (<1000) fibs)
