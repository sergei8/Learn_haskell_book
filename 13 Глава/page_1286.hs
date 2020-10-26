import Control.Monad.Writer

logNumbers :: Int -> Writer [String] Int
-- logNumbers x = writer (x, ["accepted"])
logNumbers x = writer (x, ["`" ++ show x ++ "`" ++ " accepted"])

multWithLog :: Int -> Int -> Writer [String] Int
multWithLog x y = do
  x' <- logNumbers x
  y' <- logNumbers y
  return (x' * y')
