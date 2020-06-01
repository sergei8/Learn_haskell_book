data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

findElem :: (Ord a) => a -> Tree a -> Bool
findElem _ EmptyTree = False
findElem x (Node x' left right)
    | x == x' = True
    | x > x' = findElem x right
    | x < x' = findElem x left
