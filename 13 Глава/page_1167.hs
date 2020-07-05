newtype Birds = Birds Int deriving (Show)

instance Num Birds  where
  Birds a + Birds b = Birds (a + b)
  Birds a - Birds b =
    if  (a - b) > 0 then Birds (a - b) else Birds 0

type Pole = (Birds, Birds)

-- instance Monad Pole where
--   return x = Pole x
  -- x >>= f  =

landOnLeft :: Pole -> Birds -> Pole
landOnLeft (left, right) x = (left + x, right)

landOnRight :: Pole -> Birds -> Pole
landOnRight (left, right) x = (left, right + x)

leaveFromLeft :: Pole -> Birds -> Pole
leaveFromLeft (left, right) x = (left - x, right)

leaveFromRight :: Pole -> Birds -> Pole
leaveFromRight (left, right) x = (left, right - x)
