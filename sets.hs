import Control.Monad

-- lists
sub :: Eq a => [a] -> [a] -> Bool
sub xs ys = and [ elem x ys | x <- xs ]

-- set
data Set = Set [Set] deriving Show

instance Eq Set where
    Set xs == Set ys = sub xs ys && sub ys xs

-- singleton
single :: Set -> Set
single x = Set [x]

-- pair
pair :: Set -> Set -> Set
pair x y = Set [x,y]

-- union
union :: Set -> Set
union (Set xs) = Set (concat [ ys | Set ys <- xs ])

union2 :: Set -> Set -> Set
union2 x y = union (pair x y)

-- power
power :: Set -> Set
power (Set xs) = Set (map Set (filterM (\x -> [False, True]) xs))

-- successor
successor :: Set -> Set
successor x = union2 x (single x)

-- ordinal
ordinal :: Int -> Set
ordinal 0 = Set []
ordinal n | n > 0 = successor (ordinal (n - 1))

omega = Set (map ordinal [1..])