import Data.List

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet xs = 
    foldl (\acc x ->) [] xs


compareLists :: (Ord a) => [a] -> [a] -> Bool
compareLists xs ys = sort xs == sort ys