import Data.Ord

stringCompare :: String -> String -> Ordering
stringCompare [] [] = EQ
stringCompare [] ys = LT
stringCompare xs [] = GT
stringCompare (x:xs) (y:ys)
  | x == y = stringCompare xs ys
  | x > y  = GT
  | otherwise = LT
