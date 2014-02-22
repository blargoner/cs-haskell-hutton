-- exercise 1
sumsquares :: Int -> Int
sumsquares n = sum [ x ^ 2 | x <- [1..n] ]

-- exercise 2
repl :: Int -> a -> [a]
repl n x = [ x | _ <- [1..n] ]

-- exercise 3
pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]

-- exercise 4
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], x == sum [ y | y <- factors x, y /= x ] ]

-- exercise 5
pair1 :: [a] -> [b] -> [(a,b)]
pair1 xs ys = [ (x,y) | x <- xs, y <- ys ]

pair2 :: [a] -> [b] -> [(a,b)]
pair2 xs ys = concat [ [ (x,y) | y <- ys ] | x <- xs ]

-- exercise 6
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x,y) <- zip xs ys ]
