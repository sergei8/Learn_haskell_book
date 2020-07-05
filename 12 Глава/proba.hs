-- class Monoid' m where
--   mempty :: m
--   mappend :: [m]

instance Monoid [(Int,Int)] where
  mempty     = [(0,0)]
  [(x, y)] `mappend` [(x', y')] = [(x + x', y + y')]
