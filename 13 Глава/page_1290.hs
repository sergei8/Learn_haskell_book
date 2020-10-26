import Control.Monad.Writer

euclide :: Int -> Int -> Int
euclide x y =
  maximum [x' | x' <- devidersList (abs x),
                y' <- devidersList (abs y),
                x' == y']

devidersList :: Int -> [Int]
devidersList x =
  [x' | x' <- [1..x], (x `mod` x') == 0 ]


-- euclideW' :: Int -> Int -> Writer [String] Int
-- -- euclideW' :: Int -> Int -> Writer [String] (Int, Int)
-- euclideW' x y = do
--   xs <- devidersList (abs x)
--   ys <- devidersList (abs y)
--   zs <- (xs,ys)
--   return $ writer (zs, ["result"])

-- ------------------------------------------
euclide' :: Int -> Int -> Int
euclide' x 0 = x
euclide' x y = euclide' y (x `mod` y)

euclideW :: Int -> Int -> Writer [String] Int
euclideW x 0 = writer (x, ["Result = " ++ show x])
euclideW x y = do
  tell [show x ++ " `mod` " ++ show y ++ " = " ++ show (x `mod` y)]
  euclideW y (x `mod` y)
