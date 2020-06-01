data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node x' left right)
    | x == x' = Node x left right
    | x < x'  = Node x' (treeInsert x left) right
    | x > x'  = Node x' left (treeInsert x right)
