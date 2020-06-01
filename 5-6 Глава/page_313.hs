-- найти число меньшее 100000 которое делится на 3829 без осатка
findNumber :: Integer
findNumber = maximum [ x | x <- [3829..100000], (mod x 3829) == 0 ]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' f (x:xs) = if (f x == True)
                      then x : takeWhile' f xs
                      else takeWhile' f []

-- найти сумму всех нечетных квадратов <10000
sumOddSquare :: Integer
sumOddSquare = sum [x^2 | x <- [1..10000], (x `mod` 2) > 0]

-- ряд Коллаца
colats' :: Int -> [Int]
colats' 1 = []
colats' x | even x      = x : colats' (x `div` 2)
          | otherwise   = x : colats'(3 * x + 1)

chainMore15 :: Int -> Int
chainMore15 x = length [length (colats' x) | x' <- [1..x], length (colats' x') > 15]
