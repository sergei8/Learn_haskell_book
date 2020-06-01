data Tree a = Leaf | Node { getValue :: a
                        , getLeftTree :: Tree a
                        , getRightTree :: Tree a}

class Eq' a where
  (====) :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data TrafficLight = Red | Green | Yellow

instance Eq' TrafficLight where
  Red ==== Red        = True
  Green ==== Green    = True
  Yellow ==== Yellow  = True
  _ ==== _            = False

instance Show' TrafficLight where
  show' Red = "Красный"
  show' _   = "Другой"
