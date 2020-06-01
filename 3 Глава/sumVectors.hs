type Vect = [(Int, Int)]
sumVect ::  Vect -> Vect -> Vect
sumVect [] y = y
sumVect x [] = x
-- sumVect x y = [(fst x' + fst y', snd x' + snd y') | x' <- x, y' <- y ]
sumVect x y = [(x1 + y1, x2 +y2) | (x1, x2) <- x, (y1, y2) <- y]
