maximum' :: Ord a => [a] -> a
maximum' []     = error "Пустой список"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = if n <= 0
                  then error "can not replicte"
                  else [x] ++ replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n list@(x:xs) | n <= 0 = []
                    | n >= length list = list
                    | otherwise = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []  = []
zip' [] _  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x' (x:xs) = if x' == x
                  then True
                  else elem' x' xs

-- qsort :: (Ord a) => [a] -> [a]
-- qsort []      = []
-- qsort (x:[])  = [x]
-- qsort (x:xs)  = if x <= head xs
--                 then x : qsort xs
--                 else qsort (xs ++ [x])

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort xs'@(x:xs) = smallest xs' ++ [x] ++ largest xs' where
  smallest (x:xs) = qsort [x' | x' <- xs, x' < x]
  largest (x:xs)  = qsort [x' | x' <- xs, x' >= x]
