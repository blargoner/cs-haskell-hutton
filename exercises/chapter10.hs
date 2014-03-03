-- exercise 1
data Nat = Zero | Succ Nat
            deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n | n > 0 = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add (mult m n) m

-- exercise 3
data Tree = Leaf Int | Node Tree Tree
            deriving Show

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = balanced l && balanced r && abs (leaves l - leaves r) <= 1

-- exercise 4
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs | not (null xs)  = Node (balance ls) (balance rs) where (ls, rs) = halve xs
