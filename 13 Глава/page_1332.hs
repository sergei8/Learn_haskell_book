type Stack = [Int]

pop :: Stack -> Maybe (Int, Stack)
pop [] = Nothing
pop (x:xs) = Just (x, xs)