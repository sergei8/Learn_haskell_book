newtype Stack a = Stack a deriving (Show)

class Stacked a  where 
    (/+/) :: a -> Stack [a] -> Stack [a]
    (/-/) :: Stack [a] -> Stack [a]

instance Stacked (Stack a) where
    (/+/) x (Stack xs) = Stack (x:xs) 
    (/-/) (Stack xs) = Stack (tail xs)