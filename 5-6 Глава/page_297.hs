zipWith' :: (a -> b -> c) -> [a] -> [b] ->[c]
-- zipWith' _ [] []  = []
zipWith' _ _ []   = []
zipWith' _ [] _   = []
zipWith' f (x:xs) (y:ys) = (x `f` y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
-- map' _ [] = []
-- map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' f (x:xs) | (f x == True) = x : filter' f xs
                 | otherwise     = filter' f xs
-- filter' f xs = [ x | x <- xs, f x]
