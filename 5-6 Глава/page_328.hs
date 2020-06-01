f3 :: Int -> (Int -> (Int -> Int))
f3 x y z = x + y + z

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \a b -> f b a

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y  = foldl (\acc x -> acc || (x == y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> f x : acc) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []
