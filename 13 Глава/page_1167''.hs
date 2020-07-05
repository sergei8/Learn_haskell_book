type Birds = Int
type Pole = (Birds, Birds)

main' = do
  let pole = (0, 0)
  l    <- landLeft 1 pole
  Nothing
  r    <- landRight 2 l
  return r

landLeft :: Birds -> Pole -> Maybe Pole
landLeft x p =
  if birdsLimit newPole then Just newPole else Nothing
  where
    newPole = (landNewBirds (fst p) x,  snd p)

landRight :: Birds -> Pole -> Maybe Pole
landRight x p =
  if birdsLimit newPole then Just newPole else Nothing
  where
    newPole = (fst p, landNewBirds (snd p) x)

banana :: Pole -> Maybe Pole
banana p = Nothing

birdsLimit :: Pole -> Bool
birdsLimit (l, r) = abs (l - r) < 4

-- prevent negative birds count
landNewBirds :: Int -> Int -> Int
landNewBirds p x = if (p + x) > 0 then (p + x) else 0
