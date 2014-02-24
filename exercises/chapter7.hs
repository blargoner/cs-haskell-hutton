-- exercise 1
comp1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comp1 f p xs = [ f x | x <- xs, p x ]

comp2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comp2 f p = map f . filter p

-- exercise 2
all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p

any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x : xs) | p x = x : takeWhile2 p xs
                      | otherwise = []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x : xs) | p x = dropWhile2 p xs
                      | otherwise = x : xs

-- exercise 3
map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x ys -> f x : ys) []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x ys -> if p x then x : ys else ys) []

-- exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\v d -> 10 * v + d) 0

-- exercise 6
curry2 :: ((a,b) -> c) -> (a -> b -> c)
curry2 f = \x y -> f (x,y)

uncurry2 :: (a -> b -> c) -> ((a,b) -> c)
uncurry2 f = \(x,y) -> f x y
