reverse' :: [a] -> [a]
-- reverse' = foldr (\x acc -> acc ++ [x]) []
reverse' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldl (\acc x -> if f x == True then x:acc else acc) []

last' :: [a] -> a
-- last' [] = error "empty list"
-- last' xs = (foldl (\_ x -> x) [] xs) !! 0
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
-- and' = foldr (\x acc -> x && acc) True
and' = foldr (&&) True
