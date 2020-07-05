-- import qualified Data.Foldable as F
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left ) (fmap f right)

instance Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r
