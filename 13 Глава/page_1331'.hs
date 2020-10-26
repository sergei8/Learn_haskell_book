-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}

data Stack a = Stack a deriving (Show)

-- add :: a -> Stack [a] -> Stack [a]
-- add x (Stack xs) = Stack (x:xs)

instance (Eq a) => Eq (Stack a) where
    (==) (Stack xs) (Stack ys) = xs == ys