type HorsePos = (Int, Int)

-- return all possible points achived from current position
possiblePos :: HorsePos -> [HorsePos]
possiblePos (x, y) =
  [ (x', y') | x' <- [x + 2, x - 2], y' <- [y + 1, y - 1], cond x' y'] ++
  [ (x', y') | x' <- [x + 1, x - 1], y' <- [y + 2, y - 2], cond x' y']
  where
   cond x y = x `elem` [1..8] && y `elem` [1..8]
   -- cond x y = x > 0 && x <= 8 && y > 0 && y <= 8

-- build all possible positions of 3 horse steps
possibleMoves :: HorsePos -> [HorsePos]
possibleMoves start =
  return start  >>= possiblePos >>= possiblePos >>= possiblePos
  -- [(x,y)] >>= \(x, y) -> possiblePos (x, y)
  --         >>= \(x, y) -> possiblePos (x, y)
  --         >>= \(x, y) -> possiblePos (x, y)


checkMove :: HorsePos -> HorsePos -> Bool
checkMove start end = end `elem` possibleMoves start
