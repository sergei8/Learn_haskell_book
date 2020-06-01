doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x < 100
                      then x * 2
                      else x
doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x < 100
                      then x * 2
                      else x) + 10
