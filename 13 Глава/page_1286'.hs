import Control.Monad.Writer

sumNumbers :: Int -> Writer (Sum Int) Int
sumNumbers x = writer (x, Sum x)

multWithSum :: Int -> Int -> Writer (Sum Int) Int
multWithSum x y = do
  x' <- sumNumbers x
  y' <- sumNumbers y
  tell (Sum 10)
  return (x' * y')
