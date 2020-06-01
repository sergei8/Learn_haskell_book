-- поиск в словаре по ключу

findInDict :: (Eq k) => k -> [(k, v)] -> Maybe v
findInDict y []     = Nothing
findInDict y (x:xs) = if fst x == y
                      then Just $ snd x
                      else findInDict y xs
