data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x left right) = Node (f x) (fmap f left ) (fmap f right)
