newtype Stack s = Stack [s] deriving (Show)

class Stacked s  where 
    (/+/) :: a -> s a -> s a
    pop   :: s a -> s a

instance Stacked Stack where
    (/+/) x (Stack xs) = Stack (x:xs) 
    pop (Stack []) = Stack []
    pop (Stack xs) = Stack (tail xs)
