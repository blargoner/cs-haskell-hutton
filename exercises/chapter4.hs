-- exercise 1
halve :: [a] -> ([a], [a])
halve xs | even (length xs) = (take n xs, drop n xs) where n = length xs `div` 2

-- exercise 2
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
safetail2 xs | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_ : xs) = xs

-- exercise 4
and1 :: Bool -> Bool -> Bool
and1 x y =
    if x then
        if y then True 
        else False
    else False

-- exercise 5
and2 :: Bool -> Bool -> Bool
and2 x y = if x then y else False

-- exercise 6
mult :: Num a => a -> (a -> (a -> a))
mult = \x -> (\y -> (\z -> x * y * z))
