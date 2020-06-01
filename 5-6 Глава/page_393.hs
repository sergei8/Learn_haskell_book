findList :: (Eq a) => [a] -> [a] -> Bool
findList _ [] = False
findList [] _ = False
findList xs ys = if take (length xs) ys == xs
                 then True
                 else findList xs $ tail ys
