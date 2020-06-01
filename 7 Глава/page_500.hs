-- трёхмерный вектор и несколько операций для него.»

data Vector a = V a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (V x y z) (V x' y' z') = V (x + x') (y + y') (z + z')
