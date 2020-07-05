class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' _ Nothing  = Nothing
  fmap' f (Just a) = Just (f a)

instance Functor' (Pair c) where
  fmap' f (Pair (x,y)) = Pair (f x, y)

newtype Pair b a = Pair (a,b) deriving (Show)
-- newtype Pair x y = Pair {getPair :: (x,y)}
