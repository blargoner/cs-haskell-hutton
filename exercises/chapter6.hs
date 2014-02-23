-- exercise 1
e :: Int -> Int -> Int
e m 0 | m >= 0 = 1
e m n | m >= 0, n > 0 = m * e m (n - 1)

-- exercise 3
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x : xs) = x && and2 xs

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs : xss) = xs ++ concat2 xss

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x | n > 0 = x : replicate2 (n - 1) x

element :: [a] -> Int -> a
element (x : _) 0 = x
element (x : xs) n | n > 0 = element xs (n - 1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (y : ys) = (x == y) || elem2 x ys

-- exercise 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs ys = if x <= y then x : merge (tail xs) ys else y : merge xs (tail ys)
                where
                    x = head xs
                    y = head ys

-- exercise 5
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

msort :: Ord a => [a] => [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort f) (msort s)
            where
                p = halve xs
                f = fst p
                s = snd p

-- exercise 6
sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x : xs) = x + sum2 xs

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 n [] | n > 0 = []
take2 n (x : xs) | n > 0 = x : take2 (n - 1) xs

last2 :: [a] -> a
last2 [x] = x
last2 (_ : xs) = last2 xs
