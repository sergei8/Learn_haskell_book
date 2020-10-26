import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink food
  | food == "bins"   = (" viskey", Sum 99)
  | food == "apple"  = (" wine", Sum 25)
  | otherwise        = (" milk", Sum 10)

applySum :: (Monoid m) => (Food, m) ->(Food -> (Food, m)) -> (Food, m)
applySum (food, oldSum) f  = (drink, oldSum `mappend` newSum) where
  (drink, newSum) = f food
